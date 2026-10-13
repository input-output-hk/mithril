mod signer_config;

use std::time::Duration;

use color_eyre::eyre::eyre;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::{
    DefaultTerminal, Frame,
    layout::{Constraint, Layout, Rect},
    style::{Color, Modifier, Style, Stylize},
    text::{Line, Text},
    widgets::{Block, Cell, Paragraph, Row, Table, TableState},
};
use serde_json::Value;

use signer_config::{Network, SignerConfig, SignerConfigDraft};

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let app = App::new()?;
    let terminal = ratatui::init();
    let result = app.run(terminal);
    ratatui::restore();
    result
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConfigAttribute {
    path: Vec<String>,
    display_name: String,
}

impl ConfigAttribute {
    fn value(&self, draft: &SignerConfigDraft) -> color_eyre::Result<Option<String>> {
        let mut value = serde_json::to_value(draft)?;
        for segment in &self.path {
            value = value
                .get(segment)
                .ok_or_else(|| eyre!("Attribute path for {} is missing.", self.display_name))?
                .clone();
        }
        Ok(match value {
            Value::Null => None,
            Value::String(value) => Some(value),
            Value::Number(value) => Some(value.to_string()),
            value => Some(value.to_string()),
        })
    }

    fn set_value(&self, draft: &mut SignerConfigDraft, input: &str) -> Result<(), String> {
        if input.trim().is_empty() {
            return Err("This value cannot be empty.".to_owned());
        }

        let mut value = serde_json::to_value(&*draft).map_err(|error| error.to_string())?;
        let target = value_at_path_mut(&mut value, &self.path)
            .ok_or_else(|| format!("Unable to find {} in the draft.", self.display_name))?;
        let normalized_input = if self.path.last().is_some_and(|name| name == "Network") {
            Network::ALL
                .into_iter()
                .find(|network| network.to_string().eq_ignore_ascii_case(input))
                .map(|network| network.to_string())
                .ok_or_else(|| "Use mainnet, preprod, or preview for network.".to_owned())?
        } else {
            input.to_owned()
        };
        *target = Value::String(normalized_input.clone());

        match serde_json::from_value(value.clone()) {
            Ok(updated_draft) => *draft = updated_draft,
            Err(string_error) => {
                let numeric_input = normalized_input
                    .parse::<u64>()
                    .map_err(|_| string_error.to_string())?;
                *value_at_path_mut(&mut value, &self.path).ok_or_else(|| {
                    format!("Unable to find {} in the draft.", self.display_name)
                })? = Value::from(numeric_input);
                *draft = serde_json::from_value(value).map_err(|_| string_error.to_string())?;
            }
        }
        Ok(())
    }
}

fn value_at_path_mut<'a>(value: &'a mut Value, path: &[String]) -> Option<&'a mut Value> {
    let (segment, remaining) = path.split_first()?;
    let value = value.as_object_mut()?.get_mut(segment)?;
    if remaining.is_empty() {
        Some(value)
    } else {
        value_at_path_mut(value, remaining)
    }
}

fn collect_attributes(
    value: &Value,
    path: &mut Vec<String>,
    attributes: &mut Vec<ConfigAttribute>,
) {
    if let Value::Object(object) = value {
        for (name, value) in object {
            path.push(name.clone());
            collect_attributes(value, path, attributes);
            path.pop();
        }
    } else {
        if let Some(display_name) = path.last() {
            attributes.push(ConfigAttribute {
                path: path.clone(),
                display_name: display_name.clone(),
            });
        }
    }
}

fn signer_config_attributes() -> color_eyre::Result<Vec<ConfigAttribute>> {
    let value = serde_json::to_value(SignerConfigDraft::with_defaults())?;
    let mut attributes = Vec::new();
    collect_attributes(&value, &mut Vec::new(), &mut attributes);
    Ok(attributes)
}

#[derive(Debug)]
pub struct App {
    running: bool,
    draft: SignerConfigDraft,
    attributes: Vec<ConfigAttribute>,
    selected: usize,
    editing: bool,
    input: String,
    error: Option<String>,
    default_provenance: Vec<bool>,
    finished: bool,
    config: Option<SignerConfig>,
}

impl App {
    fn new() -> color_eyre::Result<Self> {
        let draft = SignerConfigDraft::with_defaults();
        let attributes = signer_config_attributes()?;
        let default_provenance = attributes
            .iter()
            .map(|attribute| attribute.value(&draft).map(|value| value.is_some()))
            .collect::<color_eyre::Result<_>>()?;
        Ok(Self {
            running: false,
            draft,
            attributes,
            selected: 0,
            editing: false,
            input: String::new(),
            error: None,
            default_provenance,
            finished: false,
            config: None,
        })
    }

    fn run(mut self, mut terminal: DefaultTerminal) -> color_eyre::Result<()> {
        self.running = true;
        while self.running {
            let mut render_result = Ok(());
            terminal.draw(|frame| render_result = self.render(frame))?;
            render_result?;
            if event::poll(Duration::from_millis(250))? {
                self.handle_event(event::read()?)?;
            }
        }
        Ok(())
    }

    fn render(&self, frame: &mut Frame) -> color_eyre::Result<()> {
        if self.finished {
            self.render_summary(frame);
            return Ok(());
        }

        let [table_area, footer_area] =
            Layout::vertical([Constraint::Min(5), Constraint::Length(3)]).areas(frame.area());
        self.render_grid(frame, table_area)?;
        self.render_footer(frame, footer_area)?;
        Ok(())
    }

    fn render_grid(&self, frame: &mut Frame, area: Rect) -> color_eyre::Result<()> {
        let title = " SignerConfigDraft ".bold().cyan();
        let rows = self
            .attributes
            .iter()
            .enumerate()
            .map(|(index, attribute)| {
                let current_value = attribute.value(&self.draft)?;
                let value = if self.editing && index == self.selected {
                    format!("{}_", self.input)
                } else {
                    current_value.clone().unwrap_or_default()
                };
                let provenance = if current_value.is_none() {
                    String::new()
                } else if self.default_provenance[index] {
                    "default".to_owned()
                } else {
                    "user".to_owned()
                };
                Ok(Row::new([
                    Cell::from(attribute.display_name.as_str()),
                    Cell::from(value),
                    Cell::from(provenance),
                ]))
            })
            .collect::<color_eyre::Result<Vec<_>>>()?;
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
        Ok(())
    }

    fn render_footer(&self, frame: &mut Frame, area: Rect) -> color_eyre::Result<()> {
        let text = match (&self.error, self.editing, self.is_complete()?) {
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
        Ok(())
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
                Line::from(format!("new_value: {}", config.new_value)),
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
            Event::Key(key) if key.kind == KeyEventKind::Press => self.on_key_event(key)?,
            Event::Mouse(_) | Event::Resize(_, _) => {}
            _ => {}
        }
        Ok(())
    }

    fn on_key_event(&mut self, key: KeyEvent) -> color_eyre::Result<()> {
        if key.modifiers == KeyModifiers::CONTROL && matches!(key.code, KeyCode::Char('c' | 'C')) {
            self.quit();
            return Ok(());
        }

        if self.finished {
            if matches!(key.code, KeyCode::Esc | KeyCode::Char('q')) {
                self.quit();
            }
            return Ok(());
        }

        if self.editing {
            return self.on_edit_key(key);
        }

        match key.code {
            KeyCode::Esc | KeyCode::Char('q') => self.quit(),
            KeyCode::Up => self.selected = self.selected.saturating_sub(1),
            KeyCode::Down => {
                self.selected = (self.selected + 1).min(self.attributes.len().saturating_sub(1))
            }
            KeyCode::Enter => self.begin_edit()?,
            KeyCode::Char('f' | 'F') if self.is_complete()? => self.finish(),
            _ => {}
        }
        Ok(())
    }

    fn on_edit_key(&mut self, key: KeyEvent) -> color_eyre::Result<()> {
        match key.code {
            KeyCode::Esc => {
                self.editing = false;
                self.error = None;
            }
            KeyCode::Enter => {
                match self.attributes[self.selected].set_value(&mut self.draft, &self.input) {
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
        Ok(())
    }

    fn begin_edit(&mut self) -> color_eyre::Result<()> {
        self.input = self.attributes[self.selected].value(&self.draft)?.unwrap_or_default();
        self.editing = true;
        self.error = None;
        Ok(())
    }

    fn is_complete(&self) -> color_eyre::Result<bool> {
        self.attributes
            .iter()
            .map(|attribute| attribute.value(&self.draft))
            .collect::<color_eyre::Result<Vec<_>>>()
            .map(|values| values.into_iter().all(|value| value.is_some()))
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
    use std::path::PathBuf;

    #[test]
    fn draft_defaults_are_shown_but_required_fields_remain_empty() {
        let draft = SignerConfigDraft::with_defaults();

        assert!(draft.network.is_none());
        assert!(draft.kes_secret_key_path.is_none());
        assert!(draft.operational_certificate_path.is_none());
        assert_eq!(
            attribute_named("Database Directory")
                .value(&draft)
                .expect("serialized draft")
                .as_deref(),
            Some("./mithril-signer/db")
        );
        assert_eq!(
            attribute_named("Run Interval")
                .value(&draft)
                .expect("serialized draft")
                .as_deref(),
            Some("5000")
        );
    }

    #[test]
    fn network_input_is_validated_before_setting_a_value() {
        let mut draft = SignerConfigDraft::default();

        assert!(attribute_named("Network").set_value(&mut draft, "unknown").is_err());
        assert_eq!(draft.network, None);

        assert!(attribute_named("Network").set_value(&mut draft, "PREPROD").is_ok());
        assert_eq!(draft.network, Some(Network::Preprod));
    }

    #[test]
    fn display_names_are_discovered_from_draft_serde_attributes() {
        let attributes = signer_config_attributes().expect("serializable draft");
        let display_names = attributes
            .iter()
            .map(|attribute| attribute.display_name.as_str())
            .collect::<Vec<_>>();

        assert_eq!(
            display_names,
            [
                "Network",
                "KES Secret Key Path",
                "Operational Certificate Path",
                "Database Directory",
                "Cardano CLI Path",
                "Run Interval",
                "Store Retention Limit"
            ]
        );
    }

    #[test]
    fn numeric_input_is_parsed_before_setting_a_value() {
        let mut draft = SignerConfigDraft::default();

        attribute_named("Run Interval")
            .set_value(&mut draft, "not-a-number")
            .expect_err("invalid number");
        assert_eq!(draft.run_interval, None);

        attribute_named("Run Interval")
            .set_value(&mut draft, "2500")
            .expect("valid number");
        assert_eq!(draft.run_interval, Some(2500));
    }

    #[test]
    fn empty_paths_are_rejected() {
        let mut draft = SignerConfigDraft::default();

        assert!(
            attribute_named("KES Secret Key Path")
                .set_value(&mut draft, " ")
                .is_err()
        );
        assert_eq!(draft.kes_secret_key_path, None);
    }

    #[test]
    fn complete_draft_converts_to_signer_config() {
        let mut draft = SignerConfigDraft::with_defaults();
        assert!(SignerConfig::try_from(&draft).is_err());

        attribute_named("Network")
            .set_value(&mut draft, "mainnet")
            .expect("valid network");
        attribute_named("KES Secret Key Path")
            .set_value(&mut draft, "./kes.skey")
            .expect("valid path");
        attribute_named("Operational Certificate Path")
            .set_value(&mut draft, "./opcert.cert")
            .expect("valid path");

        let config = SignerConfig::try_from(&draft).expect("complete draft");
        assert_eq!(config.network, Network::Mainnet);
        assert_eq!(config.kes_secret_key_path, PathBuf::from("./kes.skey"));
        assert_eq!(config.run_interval, 5000);
    }

    fn attribute_named(display_name: &str) -> ConfigAttribute {
        signer_config_attributes()
            .expect("serializable draft")
            .into_iter()
            .find(|attribute| attribute.display_name == display_name)
            .expect("attribute exists")
    }

    #[test]
    fn nested_struct_attributes_are_discovered_recursively() {
        let nested = serde_json::json!({
            "Signer": {
                "Network": null,
                "Paths": {
                    "KES Secret Key Path": null
                }
            }
        });
        let mut attributes = Vec::new();

        collect_attributes(&nested, &mut Vec::new(), &mut attributes);

        assert_eq!(attributes.len(), 2);
        assert_eq!(attributes[0].path, ["Signer", "Network"]);
        assert_eq!(
            attributes[1].path,
            ["Signer", "Paths", "KES Secret Key Path"]
        );
    }
}
