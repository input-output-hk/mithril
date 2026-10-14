use std::{fmt, path::PathBuf};

/// Cardano network the signer runs on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Network {
    Mainnet,
    Preprod,
    Preview,
}

impl Network {
    pub const ALL: [Network; 3] = [Network::Mainnet, Network::Preprod, Network::Preview];
}

impl fmt::Display for Network {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Network::Mainnet => write!(f, "mainnet"),
            Network::Preprod => write!(f, "preprod"),
            Network::Preview => write!(f, "preview"),
        }
    }
}

/// Configuration of a Mithril signer, filled by answering the setup questions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignerConfig {
    pub network: Network,
    pub kes_secret_key_path: PathBuf,
    pub operational_certificate_path: PathBuf,
    pub db_directory: PathBuf,
    pub cardano_cli_path: PathBuf,
    pub run_interval: u64,
    pub store_retention_limit: u64,
}

/// [`SignerConfig`] being filled, each field is set once its question is answered.
#[derive(Debug, Default, Clone)]
pub struct SignerConfigDraft {
    pub network: Option<Network>,
    pub kes_secret_key_path: Option<PathBuf>,
    pub operational_certificate_path: Option<PathBuf>,
    pub db_directory: Option<PathBuf>,
    pub cardano_cli_path: Option<PathBuf>,
    pub run_interval: Option<u64>,
    pub store_retention_limit: Option<u64>,
}

impl SignerConfigDraft {
    /// Build the final [`SignerConfig`], returns `None` if a question is left unanswered.
    pub fn build(self) -> Option<SignerConfig> {
        Some(SignerConfig {
            network: self.network?,
            kes_secret_key_path: self.kes_secret_key_path?,
            operational_certificate_path: self.operational_certificate_path?,
            db_directory: self.db_directory?,
            cardano_cli_path: self.cardano_cli_path?,
            run_interval: self.run_interval?,
            store_retention_limit: self.store_retention_limit?,
        })
    }
}

/// Answer given by the user to a [`Question`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Answer {
    /// Index of the selected option.
    Choice(usize),
    Text(String),
    Integer(u64),
}

/// The kind of answer expected for a [`Question`].
#[derive(Debug, Clone)]
pub enum QuestionKind {
    /// Pick one value among a fixed list of options.
    Choice { options: Vec<String> },
    /// Free text, falling back to `default` when left empty.
    Text { default: &'static str },
    /// Positive integer, falling back to `default` (if any) when left empty.
    Integer { default: Option<u64> },
}

/// A question asked to the user, its answer is written into a [`SignerConfigDraft`] field.
#[derive(Debug, Clone)]
pub struct Question {
    pub title: &'static str,
    pub help: &'static str,
    pub kind: QuestionKind,
    apply: fn(&mut SignerConfigDraft, &Answer),
}

impl Question {
    /// Build an [`Answer`] from typed text; choice-or-text questions use it for custom values.
    pub fn parse_input(&self, raw: &str) -> Result<Answer, String> {
        let raw = raw.trim();
        match &self.kind {
            QuestionKind::Choice { .. } => {
                Err("choice questions are not answered with text".into())
            }
            QuestionKind::Text { default } => Ok(Answer::Text(if raw.is_empty() {
                default.to_string()
            } else {
                raw.to_string()
            })),
            QuestionKind::Integer { default } => match (raw.is_empty(), default) {
                (true, Some(default)) => Ok(Answer::Integer(*default)),
                (true, None) => Err("A value is required".into()),
                (false, _) => raw
                    .parse::<u64>()
                    .map(Answer::Integer)
                    .map_err(|_| format!("'{raw}' is not a valid positive integer")),
            },
        }
    }

    /// Write the answer into the matching field of the draft.
    pub fn apply(&self, draft: &mut SignerConfigDraft, answer: &Answer) {
        (self.apply)(draft, answer)
    }

    /// Human readable value of an answer to this question.
    pub fn format_answer(&self, answer: &Answer) -> String {
        match (answer, &self.kind) {
            (Answer::Choice(index), QuestionKind::Choice { options }) => {
                options.get(*index).cloned().unwrap_or_default()
            }
            (Answer::Text(value), _) => value.clone(),
            (Answer::Integer(value), _) => value.to_string(),
            (Answer::Choice(index), _) => index.to_string(),
        }
    }
}

fn as_path(answer: &Answer) -> Option<PathBuf> {
    match answer {
        Answer::Text(value) => Some(PathBuf::from(value)),
        _ => None,
    }
}

fn as_integer(answer: &Answer) -> Option<u64> {
    match answer {
        Answer::Integer(value) => Some(*value),
        _ => None,
    }
}

/// Questions needed to fill a [`SignerConfig`].
pub fn signer_questions() -> Vec<Question> {
    vec![
        Question {
            title: "Network",
            help: "Choose a network",
            kind: QuestionKind::Choice {
                options: Network::ALL.iter().map(Network::to_string).collect(),
            },
            apply: |draft, answer| {
                if let Answer::Choice(index) = answer {
                    draft.network = Network::ALL.get(*index).copied();
                }
            },
        },
        Question {
            title: "KES secret key path",
            help: "Path to your Cardano KES secret key file",
            kind: QuestionKind::Text {
                default: "/cardano/keys/kes.skey",
            },
            apply: |draft, answer| draft.kes_secret_key_path = as_path(answer),
        },
        Question {
            title: "Operational certificate path",
            help: "Path to your Cardano operational certificate file",
            kind: QuestionKind::Text {
                default: "/cardano/cert/opcert.cert",
            },
            apply: |draft, answer| draft.operational_certificate_path = as_path(answer),
        },
        Question {
            title: "DB directory",
            help: "Path to the database folder of the Cardano node",
            kind: QuestionKind::Text {
                default: "/cardano/db",
            },
            apply: |draft, answer| draft.db_directory = as_path(answer),
        },
        Question {
            title: "cardano-cli path",
            help: "Path to the cardano-cli executable",
            kind: QuestionKind::Text {
                default: "/app/bin/cardano-cli",
            },
            apply: |draft, answer| draft.cardano_cli_path = as_path(answer),
        },
        Question {
            title: "Run interval",
            help: "Interval between two runs of the signer state machine (in milliseconds)",
            kind: QuestionKind::Integer { default: None },
            apply: |draft, answer| draft.run_interval = as_integer(answer),
        },
        Question {
            title: "Store retention limit",
            help: "Number of records to keep in the signer stores",
            kind: QuestionKind::Integer { default: None },
            apply: |draft, answer| draft.store_retention_limit = as_integer(answer),
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn question(kind: QuestionKind) -> Question {
        Question {
            title: "title",
            help: "help",
            kind,
            apply: |_, _| {},
        }
    }

    #[test]
    fn text_input_falls_back_to_default_when_empty() {
        let q = question(QuestionKind::Text {
            default: "/default",
        });

        assert_eq!(Ok(Answer::Text("/default".into())), q.parse_input("  "));
        assert_eq!(Ok(Answer::Text("/custom".into())), q.parse_input("/custom"));
    }

    #[test]
    fn integer_input_is_validated() {
        let q = question(QuestionKind::Integer { default: None });

        assert_eq!(Ok(Answer::Integer(42)), q.parse_input("42"));
        assert!(q.parse_input("").is_err());
        assert!(q.parse_input("abc").is_err());
        assert!(q.parse_input("-1").is_err());
    }

    #[test]
    fn integer_input_falls_back_to_default_when_empty() {
        let q = question(QuestionKind::Integer { default: Some(10) });

        assert_eq!(Ok(Answer::Integer(10)), q.parse_input(""));
    }

    #[test]
    fn choice_or_text_accepts_custom_text_and_rejects_empty_input() {
        let q = question(QuestionKind::ChoiceOrText {
            options: vec!["first".into(), "second".into()],
        });

        assert_eq!(Ok(Answer::Text("custom".into())), q.parse_input(" custom "));
        assert!(q.parse_input("  ").is_err());
        assert_eq!("second", q.format_answer(&Answer::Choice(1)));
    }

    #[test]
    fn answering_all_questions_builds_a_signer_config() {
        let answers = [
            Answer::Choice(1),
            Answer::Text("/kes.skey".into()),
            Answer::Text("/opcert.cert".into()),
            Answer::Text("/db".into()),
            Answer::Text("/cardano-cli".into()),
            Answer::Integer(60000),
            Answer::Integer(5),
            Answer::Text("custom value".into()),
        ];
        let mut draft = SignerConfigDraft::default();
        for (question, answer) in signer_questions().iter().zip(&answers) {
            question.apply(&mut draft, answer);
        }

        assert_eq!(
            Some(SignerConfig {
                network: Network::Preprod,
                kes_secret_key_path: "/kes.skey".into(),
                operational_certificate_path: "/opcert.cert".into(),
                db_directory: "/db".into(),
                cardano_cli_path: "/cardano-cli".into(),
                run_interval: 60000,
                store_retention_limit: 5,
                user_choice_or_text_input: "custom value".into(),
            }),
            draft.build()
        );
    }

    #[test]
    fn draft_with_missing_answer_does_not_build() {
        assert_eq!(None, SignerConfigDraft::default().build());
    }
}
