use serenity::model::id::{UserId, ChannelId};
use serenity::prelude::{
    Context,
    Mentionable,
};
use diesel::prelude::*;
use crate::{
    schema,
    CetrizineError,
    ContextExt,
    SmartHax,
};
use std::convert::TryInto;

const EIGHTBALL_MESSAGES:&'static [&'static str] = &[
    "Winners pick snippers!",
    "I'd prefer something solid.",
    "Be thin and flexible.",
    "Cut to the chase.",
    "Hard to beat rock.",
    "Take note.",
    "Paper will never see it coming.",
    "Scissors deserve to be bent.",
    "You've got this covered. \"this\" being a rock. Cover the rock.",
    "Yeet!",
    "Yesn't.",
    "You have bigger problems to worry about.",
    "Git gud.",
    "You could steal the line that bop-it uses when you lose: \"do it the same but, uh, better\"",
    "Pick whichever one you least think you should pick after reading this message."
];

pub fn start_game(
    ctx: &Context, 
    challenger_id: UserId, 
    receiver_id: UserId, 
    game_location: ChannelId,
    barely_randomness: u64,
) -> Result<(), CetrizineError> {
    use serenity::model::channel::ReactionType;
    use schema::rps_game::dsl;
    let eightball_msg = EIGHTBALL_MESSAGES[(barely_randomness % EIGHTBALL_MESSAGES.len().try_into().unwrap():u64) as usize];
    let common_text = format!(
        "to a rock-paper-scissors duel! Pick your weapon: (\u{1F5FF} is what's used for \"rock\", there really isn't anything closer, sorry.)\n\nWisdom: {}",
        eightball_msg,
    );
    let challenger_channel = challenger_id.create_dm_channel(&ctx)?;
    let challenger_msg = challenger_channel.send_message(&ctx, |cm|
        cm.content(format!( 
            "You challenged {} {}", 
            receiver_id.mention(),
            &common_text,
        ))
    )?;

    let receiver_channel = receiver_id.create_dm_channel(&ctx)?;
    let receiver_msg = receiver_channel.send_message(&ctx, |cm|
        cm.content(format!( 
            "You have been challenged by {} {}", 
            challenger_id.mention(),
            &common_text,
        ))
    )?;

    let mr_lonely = if challenger_id == receiver_id {
        "\nhttps://www.youtube.com/watch?v=saZ-peC6rFc"
    } else {""};

    let conn = ctx.get_pool_arc().get()?;
    let rowid:i64 = diesel::insert_into(dsl::rps_game).values((
        dsl::game_location_channel_id.eq(SmartHax(game_location)),
        dsl::challenger_user_id.eq(SmartHax(challenger_id)),
        dsl::receiver_user_id.eq(SmartHax(receiver_id)),
        dsl::challenger_private_message_id.eq(SmartHax(challenger_msg.id)),
        dsl::receiver_private_message_id.eq(SmartHax(receiver_msg.id)),
    )).returning(dsl::rowid).get_result(&*conn)?;
    game_location.say(
        &ctx,
        format!(
            "{} has challenged {} to a rock-paper-scissors duel!\nWisdom: {}\nGame #{}{}", 
            challenger_id.mention(),
            receiver_id.mention(),
            eightball_msg,
            rowid,
            mr_lonely,
        )
    )?;
    for emoji in ["\u{1f5ff}","\u{1f4f0}","\u{2702}"].iter() {
        for msg in [&challenger_msg,&receiver_msg].iter() {
            msg.react(ctx, *emoji)?;
        }
    }
    Ok(())
}