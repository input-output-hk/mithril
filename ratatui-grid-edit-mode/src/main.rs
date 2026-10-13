mod signer_config;

use std::{path::PathBuf, time::Duration};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::{
    DefaultTerminal, Frame,
    layout::{Constraint, Layout, Rect},
    style::{Color, Modifier, Style, Stylize},
    text::{Line, Text},
    widgets::{Block, Cell, Paragraph, Row, Table, TableState},
};

use signer_config::{Network, SignerConfig, SignerConfigDraft};

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let terminal = ratatui::init();
    let result = App::new().run(terminal);
    ratatui::restore();
    result
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConfigField {
    Network,
    KesSecretKeyPath,
    OperationalCertificatePath,
    DbDirectory,
    CardanoCliPath,
    RunInterval,
    StoreRetentionLimit,
}

impl ConfigField {
    const ALL: [Self; 7] = [
        Self::Network,
        Self::KesSecretKeyPath,
        Self::OperationalCertificatePath,
        Self::DbDirectory,
        Self::CardanoCliPath,
        Self::RunInterval,
        Self::StoreRetentionLimit,
    ];

    fn name(self) -> &'static str {
        match self {
            Self::Network => "network",
            Self::KesSecretKeyPath => "kes_secret_key_path",
            Self::OperationalCertificatePath => "operational_certificate_path",
            Self::DbDirectory => "db_directory",
            Self::CardanoCliPath => "cardano_cli_path",
            Self::RunInterval => "run_interval",
            Self::StoreRetentionLimit => "store_retention_limit",
        }
    }

    fn value(self, draft: &SignerConfigDraft) -> Option<String> {
        match self {
            Self::Network => draft.network.map(|value| value.to_string()),
            Self::KesSecretKeyPath => draft
                .kes_secret_key_path
                .as_ref()
                .map(|value| value.display().to_string()),
            Self::OperationalCertificatePath => draft
                .operational_certificate_path
                .as_ref()
                .map(|value| value.display().to_string()),
            Self::DbDirectory => {
                draft.db_directory.as_ref().map(|value| value.display().to_string())
            }
            Self::CardanoCliPath => draft
                .cardano_cli_path
                .as_ref()
                .map(|value| value.display().to_string()),
            Self::RunInterval => draft.run_interval.map(|value| value.to_string()),
            Self::StoreRetentionLimit => draft.store_retention_limit.map(|value| value.to_string()),
        }
    }

    fn set_value(self, draft: &mut SignerConfigDraft, value: &str) -> Result<(), String> {
        match self {
            Self::Network => {
                draft.network = Some(
                    Network::ALL
                        .into_iter()
                        .find(|network| network.to_string().eq_ignore_ascii_case(value))
                        .ok_or_else(|| {
                            "Use mainnet, preprod, or preview for network.".to_owned()
                        })?,
                );
            }
            Self::KesSecretKeyPath => {
                draft.kes_secret_key_path = Some(parse_path(value)?);
            }
            Self::OperationalCertificatePath => {
                draft.operational_certificate_path = Some(parse_path(value)?);
            }
            Self::DbDirectory => {
                draft.db_directory = Some(parse_path(value)?);
            }
            Self::CardanoCliPath => {
                draft.cardano_cli_path = Some(parse_path(value)?);
            }
            Self::RunInterval => {
                draft.run_interval =
                    Some(value.parse().map_err(|_| {
                        "Enter a non-negative integer for run_interval.".to_owned()
                    })?);
            }
            Self::StoreRetentionLimit => {
                draft.store_retention_limit = Some(value.parse().map_err(|_| {
                    "Enter a non-negative integer for store_retention_limit.".to_owned()
                })?);
            }
        }
        Ok(())
    }
}

fn parse_path(value: &str) -> Result<PathBuf, String> {
    if value.trim().is_empty() {
        Err("This path cannot be empty.".into())
    } else {
        Ok(PathBuf::from(value))
    }
}

#[derive(Debug)]
pub struct App {
    running: bool,
    draft: SignerConfigDraft,
    selected: usize,
    editing: bool,
    input: String,
    error: Option<String>,
    default_provenance: [bool; ConfigField::ALL.len()],
    finished: bool,
    config: Option<SignerConfig>,
}

impl Default for App {
    fn default() -> Self {
        let draft = SignerConfigDraft::with_defaults();
        let default_provenance = ConfigField::ALL.map(|field| field.value(&draft).is_some());
        Self {
            running: false,
            draft,
            selected: 0,
            editing: false,
            input: String::new(),
            error: None,
            default_provenance,
            finished: false,
            config: None,
        }
    }
}

impl App {
    fn new() -> Self {
        Self::default()
    }

    fn run(mut self, mut terminal: DefaultTerminal) -> color_eyre::Result<()> {
        self.running = true;
        while self.running {
            terminal.draw(|frame| self.render(frame))?;
            if event::poll(Duration::from_millis(250))? {
                self.handle_event(event::read()?)?;
            }
        }
        Ok(())
    }

    fn render(&self, frame: &mut Frame) {
        if self.finished {
            self.render_summary(frame);
            return;
        }

        let [table_area, footer_area] =
            Layout::vertical([Constraint::Min(5), Constraint::Length(3)]).areas(frame.area());
        self.render_grid(frame, table_area);
        self.render_footer(frame, footer_area);
    }

    fn render_grid(&self, frame: &mut Frame, area: Rect) {
        let title = " SignerConfigDraft ".bold().cyan();
        let rows = ConfigField::ALL.iter().enumerate().map(|(index, field)| {
            let value = if self.editing && index == self.selected {
                format!("{}_", self.input)
            } else {
                field.value(&self.draft).unwrap_or_default()
            };
            let provenance = if field.value(&self.draft).is_none() {
                String::new()
            } else if self.default_provenance[index] {
                "default".to_owned()
            } else {
                "user".to_owned()
            };
            Row::new([Cell::from(field.name()), Cell::from(value), Cell::from(provenance)])
        });
        let widths = [
            Constraint::Percentage(38),
            Constraint::Percentage(47),
            Constraint::Percentage(15),
        ];
        let table = Table::new(rows, widths)
            .header(
                Row::new(["Attribute", "Value", "Provenance"])
                    .style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            )
            .row_highlight_style(Style::default().bg(Color::DarkGray).add_modifier(Modifier::BOLD))
            .block(Block::bordered().title(title));
        let mut state = TableState::default();
        state.select(Some(self.selected));
        frame.render_stateful_widget(table, area, &mut state);
    }

    fn render_footer(&self, frame: &mut Frame, area: Rect) {
        let text = match (&self.error, self.editing, self.is_complete()) {
            (Some(error), true, _) => format!("{error}  |  Enter: save  Esc: cancel"),
            (Some(error), false, _) => format!("{error}  |  Enter: edit selected row"),
            (None, true, _) => "Type a value  |  Enter: save  Esc: cancel".to_owned(),
            (None, false, true) => "↑/↓: select  Enter: edit  f: finish  q: quit".to_owned(),
            (None, false, false) => {
                "↑/↓: select  Enter: edit  Fill every value to finish  q: quit".to_owned()
            }
        };
        frame.render_widget(
            Paragraph::new(text)
                .block(Block::bordered().title(" Controls "))
                .style(Style::default().fg(Color::Gray)),
            area,
        );
    }

    fn render_summary(&self, frame: &mut Frame) {
        let summary = match &self.config {
            Some(config) => Text::from(vec![
                Line::from(format!("network: {}", config.network)),
                Line::from(format!(
                    "kes_secret_key_path: {}",
                    config.kes_secret_key_path.display()
                )),
                Line::from(format!(
                    "operational_certificate_path: {}",
                    config.operational_certificate_path.display()
                )),
                Line::from(format!("db_directory: {}", config.db_directory.display())),
                Line::from(format!(
                    "cardano_cli_path: {}",
                    config.cardano_cli_path.display()
                )),
                Line::from(format!("run_interval: {}", config.run_interval)),
                Line::from(format!(
                    "store_retention_limit: {}",
                    config.store_retention_limit
                )),
            ]),
            None => Text::from("Unable to build the signer configuration."),
        };
        frame.render_widget(
            Paragraph::new(summary)
                .block(Block::bordered().title(" Signer configuration summary "))
                .style(Style::default().fg(Color::White)),
            frame.area(),
        );
    }

    fn handle_event(&mut self, event: Event) -> color_eyre::Result<()> {
        match event {
            Event::Key(key) if key.kind == KeyEventKind::Press => self.on_key_event(key),
            Event::Mouse(_) | Event::Resize(_, _) => {}
            _ => {}
        }
        Ok(())
    }

    fn on_key_event(&mut self, key: KeyEvent) {
        if key.modifiers == KeyModifiers::CONTROL && matches!(key.code, KeyCode::Char('c' | 'C')) {
            self.quit();
            return;
        }

        if self.finished {
            if matches!(key.code, KeyCode::Esc | KeyCode::Char('q')) {
                self.quit();
            }
            return;
        }

        if self.editing {
            self.on_edit_key(key);
            return;
        }

        match key.code {
            KeyCode::Esc | KeyCode::Char('q') => self.quit(),
            KeyCode::Up => self.selected = self.selected.saturating_sub(1),
            KeyCode::Down => self.selected = (self.selected + 1).min(ConfigField::ALL.len() - 1),
            KeyCode::Enter => self.begin_edit(),
            KeyCode::Char('f' | 'F') if self.is_complete() => self.finish(),
            _ => {}
        }
    }

    fn on_edit_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Esc => {
                self.editing = false;
                self.error = None;
            }
            KeyCode::Enter => {
                let field = ConfigField::ALL[self.selected];
                match field.set_value(&mut self.draft, &self.input) {
                    Ok(()) => {
                        self.default_provenance[self.selected] = false;
                        self.editing = false;
                        self.error = None;
                    }
                    Err(error) => self.error = Some(error),
                }
            }
            KeyCode::Backspace => {
                self.input.pop();
                self.error = None;
            }
            KeyCode::Char(character)
                if key.modifiers.is_empty() || key.modifiers == KeyModifiers::SHIFT =>
            {
                self.input.push(character);
                self.error = None;
            }
            _ => {}
        }
    }

    fn begin_edit(&mut self) {
        self.input = ConfigField::ALL[self.selected].value(&self.draft).unwrap_or_default();
        self.editing = true;
        self.error = None;
    }

    fn is_complete(&self) -> bool {
        ConfigField::ALL
            .iter()
            .all(|field| field.value(&self.draft).is_some())
    }

    fn finish(&mut self) {
        match SignerConfig::try_from(&self.draft) {
            Ok(config) => {
                self.config = Some(config);
                self.finished = true;
            }
            Err(error) => self.error = Some(error.to_owned()),
        }
    }

    fn quit(&mut self) {
        self.running = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn draft_defaults_are_shown_but_required_fields_remain_empty() {
        let draft = SignerConfigDraft::with_defaults();

        assert!(draft.network.is_none());
        assert!(draft.kes_secret_key_path.is_none());
        assert!(draft.operational_certificate_path.is_none());
        assert_eq!(
            ConfigField::DbDirectory.value(&draft).as_deref(),
            Some("./mithril-signer/db")
        );
        assert_eq!(
            ConfigField::RunInterval.value(&draft).as_deref(),
            Some("5000")
        );
    }

    #[test]
    fn network_input_is_validated_before_setting_a_value() {
        let mut draft = SignerConfigDraft::default();

        assert!(ConfigField::Network.set_value(&mut draft, "unknown").is_err());
        assert_eq!(draft.network, None);

        assert!(ConfigField::Network.set_value(&mut draft, "PREPROD").is_ok());
        assert_eq!(draft.network, Some(Network::Preprod));
    }

    #[test]
    fn empty_paths_are_rejected() {
        let mut draft = SignerConfigDraft::default();

        assert!(ConfigField::KesSecretKeyPath.set_value(&mut draft, " ").is_err());
        assert_eq!(draft.kes_secret_key_path, None);
    }

    #[test]
    fn complete_draft_converts_to_signer_config() {
        let mut draft = SignerConfigDraft::with_defaults();
        assert!(SignerConfig::try_from(&draft).is_err());

        ConfigField::Network
            .set_value(&mut draft, "mainnet")
            .expect("valid network");
        ConfigField::KesSecretKeyPath
            .set_value(&mut draft, "./kes.skey")
            .expect("valid path");
        ConfigField::OperationalCertificatePath
            .set_value(&mut draft, "./opcert.cert")
            .expect("valid path");

        let config = SignerConfig::try_from(&draft).expect("complete draft");
        assert_eq!(config.network, Network::Mainnet);
        assert_eq!(config.kes_secret_key_path, PathBuf::from("./kes.skey"));
        assert_eq!(config.run_interval, 5000);
    }
}
