use std::io;

use clap::Parser;
use ratatui::crossterm::event::{Event, KeyEvent, KeyModifiers};
use ratatui::crossterm::terminal::{EnterAlternateScreen, LeaveAlternateScreen};
use ratatui::crossterm::{self, event};
use ratatui::{
    crossterm::{event::KeyCode, execute, terminal},
    layout::{Constraint, Direction, Layout},
    prelude::CrosstermBackend,
    text::Text,
    widgets::{Block, Borders, Paragraph},
    Frame, Terminal,
};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct App {
    command_buffer: String,
    command_history: Vec<String>,
    registers: Vec<String>,
}

impl App {
    fn new() -> App {
        App {
            command_buffer: String::new(),
            command_history: Vec::new(),
            registers: vec![
                "A: 01".into(),
                "B: 00".into(),
                "C: 13".into(),
                "D: 00".into(),
                "E: 37".into(),
                "H: 00".into(),
                "L: ED".into(),
            ],
        }
    }

    fn draw(&mut self, f: &mut ratatui::Frame) {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(50), // Proportion for register display
                Constraint::Percentage(50), // Proportion for command input and history
            ])
            .split(size);

        let registers_paragraph = Paragraph::new(self.registers.join("\n"))
            .block(Block::default().title("Registers").borders(Borders::ALL));
        f.render_widget(registers_paragraph, chunks[0]);

        // Build command history display
        let mut command_display = vec![format!("(gbdb) {}", self.command_buffer)];
        for command in self.command_history.iter() {
            command_display.push(format!("(gbdb) {}", command));
        }

        command_display.reverse();
        let command_history_paragraph = Paragraph::new(command_display.join("\n")).block(
            Block::default()
                .title("Command History")
                .borders(Borders::ALL),
        );
        f.render_widget(command_history_paragraph, chunks[1]);
    }

    fn run(&mut self) -> std::io::Result<()> {
        terminal::enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;
        terminal.clear()?;

        loop {
            terminal.draw(|f| self.draw(f))?;

            if let Event::Key(KeyEvent {
                code, modifiers, ..
            }) = event::read()?
            {
                match code {
                    KeyCode::Char('c') | KeyCode::Char('d')
                        if modifiers.contains(KeyModifiers::CONTROL) =>
                    {
                        break;
                    }
                    KeyCode::Char(c) if modifiers == KeyModifiers::NONE => {
                        self.command_buffer.push(c);
                    }
                    KeyCode::Backspace => {
                        self.command_buffer.pop();
                    }
                    KeyCode::Enter => {
                        self.command_history.push(self.command_buffer.clone());
                        // TODO: do something
                        self.command_buffer.clear();
                    }
                    KeyCode::Esc => {
                        break;
                    }
                    _ => {}
                }
            }
        }

        terminal::disable_raw_mode()?;
        execute!(io::stdout(), LeaveAlternateScreen)?;

        Ok(())
    }
}

#[derive(Debug, Clone, Parser)]
struct Args {
    #[arg(short, long)]
    rom_path: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut app = App::new();
    app.run()?;

    Ok(())
}
