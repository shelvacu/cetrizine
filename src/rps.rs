use serenity::model::id::{UserId, ChannelId};
use serenity::prelude::{
    Context,
    Mentionable,
};
use diesel::prelude::*;
use crate::{
    schema,
    CetrizineError,
    error::CustomError,
    ContextExt,
    SmartHax,
    SmartHaxO,
};
use std::convert::TryInto;

const EIGHTBALL_MESSAGES:&[&str] = &[
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
    let my_id = crate::USER_ID.load(std::sync::atomic::Ordering::Relaxed);
    if challenger_id == my_id {
        error!("Received a challenge from myself! This should never happen. Game_location: {:?}", game_location);
        return Err("Received a challenge from myself! This should never happen.".to_string().into());
    }
    let is_vs_me = receiver_id == my_id;
    let eightball_msg = EIGHTBALL_MESSAGES[(barely_randomness % EIGHTBALL_MESSAGES.len().try_into().unwrap():u64) as usize];
    let common_text = format!(
        "to a rock-paper-scissors duel! Pick your weapon: (\u{1F5FF} is what's used for \"rock\", there really isn't anything closer, sorry.)\n\nWisdom: {}",
        eightball_msg,
    );
    let receiver_name = if is_vs_me {
        "*me*".to_string()
    } else {
        receiver_id.mention()
    };
    let maybe_choice = if is_vs_me {
        Some(["Rock", "Paper", "Scissors"][(barely_randomness % 3) as usize])
    } else { None };
    let challenger_channel = challenger_id.create_dm_channel(&ctx)?;
    let challenger_msg = challenger_channel.send_message(&ctx, |cm|
        cm.content(format!( 
            "You challenged {} {}", 
            receiver_name,
            &common_text,
        ))
    )?;

    let mut maybe_receiver_msg = None;
    let mut maybe_receiver_channel = None;
    if !is_vs_me {
        let receiver_channel = receiver_id.create_dm_channel(&ctx)?;
        let receiver_msg = receiver_channel.send_message(&ctx, |cm|
            cm.content(format!( 
                "You have been challenged by {} {}", 
                challenger_id.mention(),
                &common_text,
            ))
        )?;
        maybe_receiver_channel = Some(receiver_channel);
        maybe_receiver_msg = Some(receiver_msg);
    };

    let mr_lonely = if challenger_id == receiver_id {
        "\nhttps://www.youtube.com/watch?v=saZ-peC6rFc"
    } else {""};

    let conn = ctx.get_pool_arc().get()?;
    let rowid:i64 = diesel::insert_into(dsl::rps_game).values((
        dsl::game_location_channel_id.eq(SmartHax(game_location)),
        dsl::challenger_user_id.eq(SmartHax(challenger_id)),
        dsl::receiver_user_id.eq(SmartHax(receiver_id)),
        dsl::challenger_private_message_id.eq(SmartHax(challenger_msg.id)),
        dsl::receiver_private_message_id.eq(SmartHaxO(maybe_receiver_msg.as_ref().map(|m| m.id))),
        dsl::receiver_choice.eq(maybe_choice),
        dsl::receiver_wants_rematch.eq(is_vs_me),
    )).returning(dsl::rowid).get_result(&*conn)?;
    if game_location == challenger_channel.id {
        //this game is happening in DMs
        challenger_channel.say(&ctx, format!("Game #{}", rowid))?;
        if let Some(receiver_channel) = maybe_receiver_channel {
            if receiver_channel.id != challenger_channel.id {
                receiver_channel.say(&ctx, format!("Game #{}", rowid))?;
            }
        }
    } else {
        game_location.say(
            &ctx,
            format!(
                "{} has challenged {} to a rock-paper-scissors duel!\nWisdom: {}\nGame #{}{}", 
                challenger_id.mention(),
                receiver_name,
                eightball_msg,
                rowid,
                mr_lonely,
            )
        )?;
    }
    for emoji in ["\u{1f5ff}","\u{1f4f0}","\u{2702}"].iter() {
        challenger_msg.react(ctx, *emoji)?;
        if let Some(receiver_msg) = &maybe_receiver_msg {
            receiver_msg.react(ctx, *emoji)?;
        }
    }
    Ok(())
}