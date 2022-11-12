/*
table 71-72

*/
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a>{
    X,
    Y(&'a str)
}

