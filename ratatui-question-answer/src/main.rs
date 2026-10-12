mod input;
mod question;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::{
    DefaultTerminal, Frame,
    layout::{Constraint, Layout, Position, Rect},
    style::{Style, Stylize},
    text::{Line, Span},
    widgets::{Block, Gauge, List, ListState, Paragraph, Row, Table, Wrap},
};

use input::TextInput;
use question::{Answer, Question, QuestionKind, SignerConfig, SignerConfigDraft, signer_questions};

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let terminal = ratatui::init();
    let result = App::new(signer_questions()).run(terminal);
    ratatui::restore();

    match result? {
        Some(config) => println!("{config:#?}"),
        None => println!("Setup aborted"),
    }
    Ok(())
}

/// Question/response wizard filling a [`SignerConfig`].
#[derive(Debug)]
pub struct App {
    /// Is the application running?
    running: bool,
    questions: Vec<Question>,
    /// Answers given so far, kept to restore them when navigating back.
    answers: Vec<Option<Answer>>,
    /// Index of the current question, equals `questions.len()` on the summary screen.
    step: usize,
    list_state: ListState,
    input: TextInput,
    error: Option<String>,
    /// Config built once the user confirms the summary.
    config: Option<SignerConfig>,
}

impl App {
    /// Construct a new instance of [`App`].
    pub fn new(questions: Vec<Question>) -> Self {
        let mut app = Self {
            running: false,
            answers: vec![None; questions.len()],
            questions,
            step: 0,
            list_state: ListState::default(),
            input: TextInput::default(),
            error: None,
            config: None,
        };
        app.load_step();
        app
    }

    /// Run the application's main loop, returns the config if the user confirmed it.
    pub fn run(mut self, mut terminal: DefaultTerminal) -> color_eyre::Result<Option<SignerConfig>> {
        self.running = true;
        while self.running {
            terminal.draw(|frame| self.render(frame))?;
            self.handle_crossterm_events()?;
        }
        Ok(self.config)
    }

    fn current_question(&self) -> Option<&Question> {
        self.questions.get(self.step)
    }

    fn is_summary(&self) -> bool {
        self.step == self.questions.len()
    }

    /// Reset the widgets state for the current step, restoring a previous answer if any.
    fn load_step(&mut self) {
        self.error = None;
        let previous = self.answers.get(self.step).cloned().flatten();
        match (self.current_question().map(|q| &q.kind), previous) {
            (Some(QuestionKind::Choice { .. }), Some(Answer::Choice(index))) => {
                self.list_state.select(Some(index))
            }
            (Some(QuestionKind::Choice { .. }), _) => self.list_state.select(Some(0)),
            (Some(QuestionKind::ChoiceOrText { .. }), Some(Answer::Choice(index))) => {
                self.list_state.select(Some(index));
                self.input = TextInput::default();
            }
            (Some(QuestionKind::ChoiceOrText { options }), Some(Answer::Text(value))) => {
                self.list_state.select(Some(options.len()));
                self.input = TextInput::with_value(value);
            }
            (Some(QuestionKind::ChoiceOrText { .. }), _) => {
                self.list_state.select(Some(0));
                self.input = TextInput::default();
            }
            (Some(_), Some(Answer::Text(value))) => self.input = TextInput::with_value(value),
            (Some(_), Some(Answer::Integer(value))) => {
                self.input = TextInput::with_value(value.to_string())
            }
            _ => self.input = TextInput::default(),
        }
    }

    /// Validate the current step and move to the next one.
    fn submit(&mut self) {
        if self.is_summary() {
            self.confirm();
            return;
        }

        let question = &self.questions[self.step];
        let answer = match &question.kind {
            QuestionKind::Choice { .. } => Ok(Answer::Choice(self.list_state.selected().unwrap_or(0))),
            QuestionKind::ChoiceOrText { options } => match self.list_state.selected() {
                Some(index) if index < options.len() => Ok(Answer::Choice(index)),
                Some(index) if index == options.len() => question.parse_input(self.input.value()),
                _ => Err("Select an option or enter a custom value".into()),
            },
            _ => question.parse_input(self.input.value()),
        };
        match answer {
            Ok(answer) => {
                self.answers[self.step] = Some(answer);
                self.step += 1;
                self.load_step();
            }
            Err(error) => self.error = Some(error),
        }
    }

    /// Go back to the previous step, quit if already on the first one.
    fn back(&mut self) {
        if self.step == 0 {
            self.quit();
        } else {
            self.step -= 1;
            self.load_step();
        }
    }

    /// Fill the config from the answers and stop the application.
    fn confirm(&mut self) {
        let mut draft = SignerConfigDraft::default();
        for (question, answer) in self.questions.iter().zip(&self.answers) {
            if let Some(answer) = answer {
                question.apply(&mut draft, answer);
            }
        }
        match draft.build() {
            Some(config) => {
                self.config = Some(config);
                self.quit();
            }
            None => self.error = Some("Some questions are not answered".into()),
        }
    }

    /// Renders the user interface.
    fn render(&mut self, frame: &mut Frame) {
        let [header_area, body_area, error_area, help_area] = Layout::vertical([
            Constraint::Length(3),
            Constraint::Min(7),
            Constraint::Length(1),
            Constraint::Length(1),
        ])
        .margin(1)
        .areas(frame.area());

        self.render_header(frame, header_area);
        match self.current_question().cloned() {
            Some(question) => self.render_question(frame, body_area, &question),
            None => self.render_summary(frame, body_area),
        }
        if let Some(error) = &self.error {
            frame.render_widget(Line::from(error.as_str()).red(), error_area);
        }
        self.render_help(frame, help_area);
    }

    fn render_header(&self, frame: &mut Frame, area: Rect) {
        let total = self.questions.len();
        let label = if self.is_summary() {
            "Summary".to_string()
        } else {
            format!("{}/{total}", self.step + 1)
        };
        let title = Line::from(" Mithril signer setup ").bold().blue();
        let gauge = Gauge::default()
            .block(Block::bordered().title(title))
            .gauge_style(Style::new().blue())
            .ratio(self.step as f64 / total as f64)
            .label(label);
        frame.render_widget(gauge, area);
    }

    fn render_question(&mut self, frame: &mut Frame, area: Rect, question: &Question) {
        let block = Block::bordered().title(Line::from(format!(" {} ", question.title)).bold());
        let inner = block.inner(area);
        frame.render_widget(block, area);

        let [help_area, _, input_area] =
            Layout::vertical([Constraint::Length(1), Constraint::Length(1), Constraint::Min(3)])
                .areas(inner);
        frame.render_widget(
            Paragraph::new(question.help).wrap(Wrap { trim: true }),
            help_area,
        );

        match &question.kind {
            QuestionKind::Choice { options } => {
                let list = List::new(options.iter().map(String::as_str))
                    .highlight_style(Style::new().bold().yellow())
                    .highlight_symbol(">> ");
                frame.render_stateful_widget(list, input_area, &mut self.list_state);
            }
            QuestionKind::ChoiceOrText { options } => {
                let list = List::new(
                    options
                        .iter()
                        .map(String::as_str)
                        .chain(std::iter::once("Custom value")),
                )
                .highlight_style(Style::new().bold().yellow())
                .highlight_symbol(">> ");
                if self.list_state.selected() == Some(options.len()) {
                    let [options_area, custom_input_area] =
                        Layout::vertical([Constraint::Min(1), Constraint::Length(3)])
                            .areas(input_area);
                    frame.render_stateful_widget(list, options_area, &mut self.list_state);
                    let content = if self.input.value().is_empty() {
                        Line::from(Span::from("Enter custom value").dark_gray().italic())
                    } else {
                        Line::from(self.input.value())
                    };
                    frame.render_widget(
                        Paragraph::new(content).block(Block::bordered()),
                        custom_input_area,
                    );
                    frame.set_cursor_position(Position::new(
                        custom_input_area.x + 1 + self.input.cursor() as u16,
                        custom_input_area.y + 1,
                    ));
                } else {
                    frame.render_stateful_widget(list, input_area, &mut self.list_state);
                }
            }
            QuestionKind::Text { .. } | QuestionKind::Integer { .. } => {
                let [input_area] = Layout::vertical([Constraint::Length(3)]).areas(input_area);
                let default = match &question.kind {
                    QuestionKind::Text { default } => Some(default.to_string()),
                    QuestionKind::Integer { default } => default.map(|d| d.to_string()),
                    QuestionKind::Choice { .. } | QuestionKind::ChoiceOrText { .. } => None,
                };
                let content = if self.input.value().is_empty() {
                    match default {
                        Some(default) => Line::from(Span::from(default).dark_gray().italic()),
                        None => Line::default(),
                    }
                } else {
                    Line::from(self.input.value())
                };
                frame.render_widget(Paragraph::new(content).block(Block::bordered()), input_area);
                frame.set_cursor_position(Position::new(
                    input_area.x + 1 + self.input.cursor() as u16,
                    input_area.y + 1,
                ));
            }
        }
    }

    fn render_summary(&self, frame: &mut Frame, area: Rect) {
        let rows = self.questions.iter().zip(&self.answers).map(|(question, answer)| {
            let value = answer
                .as_ref()
                .map(|answer| question.format_answer(answer))
                .unwrap_or_default();
            Row::new([question.title.to_string(), value])
        });
        let table = Table::new(rows, [Constraint::Length(30), Constraint::Fill(1)])
            .header(Row::new(["Setting", "Value"]).bold().underlined())
            .block(Block::bordered().title(Line::from(" Summary ").bold()));
        frame.render_widget(table, area);
    }

    fn render_help(&self, frame: &mut Frame, area: Rect) {
        let help = match self.current_question().map(|q| &q.kind) {
            None => "Enter: confirm  Esc: back  Ctrl-C: quit",
            Some(QuestionKind::Choice { .. }) => "↑/↓: choose  Enter: next  Esc: back  Ctrl-C: quit",
            Some(QuestionKind::ChoiceOrText { .. }) => {
                "↑/↓: choose  Enter: next  type with Custom value selected  Esc: back  Ctrl-C: quit"
            }
            Some(_) => "Enter: next (empty = default)  Esc: back  Ctrl-C: quit",
        };
        frame.render_widget(Line::from(help).dark_gray().centered(), area);
    }

    /// Reads the crossterm events and updates the state of [`App`].
    fn handle_crossterm_events(&mut self) -> color_eyre::Result<()> {
        match event::read()? {
            // it's important to check KeyEventKind::Press to avoid handling key release events
            Event::Key(key) if key.kind == KeyEventKind::Press => self.on_key_event(key),
            _ => {}
        }
        Ok(())
    }

    /// Handles the key events and updates the state of [`App`].
    fn on_key_event(&mut self, key: KeyEvent) {
        match (key.modifiers, key.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
            (_, KeyCode::Esc) => self.back(),
            (_, KeyCode::Enter) => self.submit(),
            _ => match self.current_question().map(|q| q.kind.clone()) {
                Some(QuestionKind::Choice { .. }) => self.on_choice_key(key.code),
                Some(QuestionKind::ChoiceOrText { options }) => {
                    self.on_choice_or_text_key(key.code, options.len())
                }
                Some(kind @ (QuestionKind::Text { .. } | QuestionKind::Integer { .. })) => {
                    self.on_input_key(key.code, matches!(kind, QuestionKind::Integer { .. }))
                }
                None => {}
            },
        }
    }

    fn on_choice_or_text_key(&mut self, code: KeyCode, option_count: usize) {
        match code {
            KeyCode::Up => {
                let selected = self.list_state.selected().unwrap_or(0);
                self.list_state.select(Some(selected.saturating_sub(1)));
            }
            KeyCode::Down => {
                let selected = self.list_state.selected().unwrap_or(0);
                self.list_state.select(Some((selected + 1).min(option_count)));
            }
            _ if self.list_state.selected() == Some(option_count) => {
                self.on_input_key(code, false);
            }
            _ => {}
        }
    }

    fn on_choice_key(&mut self, code: KeyCode) {
        match code {
            KeyCode::Up | KeyCode::Char('k') => self.list_state.select_previous(),
            KeyCode::Down | KeyCode::Char('j') => self.list_state.select_next(),
            _ => {}
        }
    }

    fn on_input_key(&mut self, code: KeyCode, digits_only: bool) {
        match code {
            KeyCode::Char(c) if !digits_only || c.is_ascii_digit() => {
                self.input.insert(c);
                self.error = None;
            }
            KeyCode::Backspace => self.input.backspace(),
            KeyCode::Delete => self.input.delete(),
            KeyCode::Left => self.input.move_left(),
            KeyCode::Right => self.input.move_right(),
            KeyCode::Home => self.input.move_home(),
            KeyCode::End => self.input.move_end(),
            _ => {}
        }
    }

    /// Set running to false to quit the application.
    fn quit(&mut self) {
        self.running = false;
    }
}
