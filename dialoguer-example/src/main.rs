use std::io;

use dialoguer::{
    FuzzySelect, Input, Select,
    console::{Style, Term, style},
    theme::ColorfulTheme,
};

mod question;

use question::{Answer, Question, QuestionKind, SignerConfigDraft, signer_questions};

fn main() -> io::Result<()> {
    let questions = signer_questions();
    let mut answers = vec![None; questions.len()];
    let title_width = questions
        .iter()
        .map(|question| question.title.len())
        .max()
        .unwrap_or(0)
        .max("Title".len());
    let value_width = questions
        .iter()
        .map(|question| default_value(question).unwrap_or_default().len())
        .max()
        .unwrap_or(0)
        .max("Value".len());
    let provenance_width = "Provenance".len();

    let mut theme = ColorfulTheme::default();
    theme.active_item_style = Style::new().for_stderr().white().on_blue().bold();

    loop {
        let value_width = questions
            .iter()
            .zip(&answers)
            .map(|(question, answer)| {
                answer
                    .as_ref()
                    .map(|answer| question.format_answer(answer))
                    .or_else(|| default_value(question))
                    .unwrap_or_default()
                    .len()
            })
            .max()
            .unwrap_or(value_width)
            .max("Value".len());
        let rows = question_rows(
            &questions,
            &answers,
            title_width,
            value_width,
            provenance_width,
        );
        let header = format!(
            "  {:<title_width$} │ {:<value_width$} │ {:<provenance_width$}",
            "Title", "Value", "Provenance"
        );
        Term::stderr().write_line(&style(header.clone()).bold().to_string())?;

        let Some(selection) = FuzzySelect::with_theme(&theme).items(&rows).interact_opt()? else {
            break;
        };

        let question = questions
            .get(selection)
            .ok_or_else(|| io::Error::other("selected question index is out of range"))?;
        edit_question(question, &mut answers[selection], &theme)?;
    }

    let mut draft = SignerConfigDraft::default();
    for (question, answer) in questions.iter().zip(&answers) {
        if let Some(answer) = answer.clone().or_else(|| default_answer(question)) {
            question.apply(&mut draft, &answer);
        }
    }

    match draft.clone().build() {
        Some(config) => println!("{config:#?}"),
        None => println!("Incomplete signer configuration:\n{draft:#?}"),
    }

    Ok(())
}

fn question_rows(
    questions: &[Question],
    answers: &[Option<Answer>],
    title_width: usize,
    value_width: usize,
    provenance_width: usize,
) -> Vec<String> {
    questions
        .iter()
        .zip(answers)
        .map(|(question, answer)| {
            let value = answer
                .as_ref()
                .map(|answer| question.format_answer(answer))
                .or_else(|| default_value(question))
                .unwrap_or_default();
            let provenance = if answer.is_some() {
                "user"
            } else if default_value(question).is_some() {
                "default"
            } else {
                ""
            };
            format!(
                "{title:<title_width$} │ {value:<value_width$} │ {provenance:<provenance_width$}",
                title = question.title
            )
        })
        .collect()
}

fn edit_question(
    question: &Question,
    answer: &mut Option<Answer>,
    theme: &ColorfulTheme,
) -> io::Result<()> {
    let default = default_value(question);
    let current_value = answer
        .as_ref()
        .map(|answer| question.format_answer(answer))
        .or_else(|| default.clone())
        .unwrap_or_else(|| "Not set".to_owned());
    let kind = match &question.kind {
        QuestionKind::Choice { options } => format!("Choice ({})", options.join(", ")),
        QuestionKind::Text { .. } => "Text".to_owned(),
        QuestionKind::Integer { .. } => "Positive integer".to_owned(),
    };
    let provenance = if answer.is_some() {
        "user"
    } else if default.is_some() {
        "default"
    } else {
        "Not set"
    };

    let term = Term::stderr();
    term.write_line(&format!("{} — {}", question.title, question.help))?;
    term.write_line(&format!("Kind: {kind}"))?;
    term.write_line(&format!(
        "Default value: {}",
        default.as_deref().unwrap_or("None")
    ))?;
    term.write_line(&format!("Current value: {current_value}"))?;
    term.write_line(&format!("Provenance: {provenance}"))?;

    *answer = Some(read_answer(question, answer.as_ref(), theme)?);
    Ok(())
}

fn read_answer(
    question: &Question,
    current_answer: Option<&Answer>,
    theme: &ColorfulTheme,
) -> io::Result<Answer> {
    match &question.kind {
        QuestionKind::Choice { options } => {
            let current_index = current_answer.and_then(|answer| match answer {
                Answer::Choice(index) => Some(*index),
                _ => None,
            });
            let mut select = Select::with_theme(theme).items(options);
            if let Some(index) = current_index {
                select = select.default(index);
            }
            Ok(select.with_prompt(question.title).interact().map(Answer::Choice)?)
        }
        QuestionKind::Text { .. } | QuestionKind::Integer { .. } => loop {
            let existing = current_answer
                .map(|answer| question.format_answer(answer))
                .or_else(|| default_value(question));
            let mut input = Input::<String>::with_theme(theme).with_prompt(question.title);
            if let Some(existing) = existing {
                input = input.default(existing);
            }
            let raw = input.interact_text()?;
            match question.parse_input(&raw) {
                Ok(answer) => return Ok(answer),
                Err(error) => Term::stderr().write_line(&error)?,
            }
        },
    }
}

fn default_value(question: &Question) -> Option<String> {
    match &question.kind {
        QuestionKind::Choice { .. } => None,
        QuestionKind::Text { default } => Some((*default).to_owned()),
        QuestionKind::Integer { default } => default.map(|value| value.to_string()),
    }
}

fn default_answer(question: &Question) -> Option<Answer> {
    match &question.kind {
        QuestionKind::Choice { .. } => None,
        QuestionKind::Text { default } => Some(Answer::Text((*default).to_owned())),
        QuestionKind::Integer { default } => default.map(Answer::Integer),
    }
}
