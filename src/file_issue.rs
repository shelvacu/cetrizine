use std::fmt;
use failure::Error;
use graphql_client::GraphQLQuery;

type URI = String;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.json",
    query_path = "createissue.graphql",
    response_derives = "Debug",
)]
pub struct CreateIssue;

pub struct IssueData {
    id: String,
    number: i64,
    url: String,
}

#[derive(Debug)]
pub struct FileIssueError(pub String);

impl fmt::Display for FileIssueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FileIssue error: {}", self.0)
    }
}

impl std::error::Error for FileIssueError{}

fn file_issue(token: String, title: String, content: String) -> Result<IssueData, Error> {
    //let auth = std::env::var("GITHUB_TOKEN").unwrap();
    let q = CreateIssue::build_query(create_issue::Variables {
        title: Some(title),
        content: Some(content),
    });

    let client = reqwest::Client::new();
    let mut res = client.post("https://api.github.com/graphql")
        .bearer_auth(token)
        .json(&q)
        .send()?;
    let response_body: graphql_client::Response<create_issue::ResponseData> = res.json()?;
    debug!("{:?}", response_body);

    if let Some(errors) = response_body.errors {
        error!("github graphql errors:");

        for error in &errors {
            error!("{:?}", error);
        }
        return Err(FileIssueError("github graphql errors".to_string()).into());
    }

    let response_data: create_issue::ResponseData = response_body.data.expect("missing response data");
    let unwrapped = response_data.create_issue.ok_or(FileIssueError("bad response data".to_string()))?.issue.ok_or(FileIssueError("bad response data".to_string()))?;
    Ok( IssueData { id: unwrapped.id, number: unwrapped.number, url: unwrapped.url } )
}
