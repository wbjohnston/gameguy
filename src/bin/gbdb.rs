use std::io;

use libgameguy::{Emulator, Opcode, ROM_00_SIZE};

use clap::Parser;
use ratatui::crossterm::event;
use ratatui::crossterm::event::{Event, KeyEvent, KeyModifiers};
use ratatui::crossterm::terminal::{EnterAlternateScreen, LeaveAlternateScreen};
use ratatui::Terminal;
use ratatui::{
    crossterm::{event::KeyCode, execute, terminal},
    layout::{Constraint, Direction, Layout},
    prelude::CrosstermBackend,
    widgets::{Block, Borders, Paragraph},
};

struct App {
    command_buffer: String,
    command_history: Vec<String>,
    emulator: Emulator,
}

impl App {
    fn new(emulator: Emulator) -> App {
        App {
            command_buffer: String::new(),
            command_history: Vec::new(),
            emulator,
        }
    }

    fn draw(&mut self, f: &mut ratatui::Frame) {
        let size = f.size();
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(25), // Proportion for register display
                Constraint::Percentage(50), // Proportion for memory and stack display
                Constraint::Percentage(25), // Proportion for command history
            ])
            .split(size);

        // Split the middle section horizontally for memory and stack
        let memory_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(50), // Memory around PC
                Constraint::Percentage(50), // Stack around SP
            ])
            .split(chunks[1]);

        let opcode_raw = self.emulator.memory().get_u8(self.emulator.cpu().pc());
        let opcode = Opcode::from(*opcode_raw);
        let registers_paragraph = Paragraph::new(
            [
                format!("AF: {:04X}", self.emulator.cpu().af()),
                format!("BC: {:04X}", self.emulator.cpu().bc()),
                format!("DE: {:04X}", self.emulator.cpu().de()),
                format!("HL: {:04X}", self.emulator.cpu().hl()),
                format!("SP: {:04X}", self.emulator.cpu().sp()),
                format!("PC: {:04X}", self.emulator.cpu().pc()),
                format!("OP: {:02X} {} ", opcode_raw, opcode),
            ]
            .join("\n"),
        )
        .block(Block::default().title("Registers").borders(Borders::ALL));
        f.render_widget(registers_paragraph, chunks[0]);

        // Memory view around PC
        let area_around_pc = self.emulator.cpu().pc().saturating_sub(16)
            ..self.emulator.cpu().pc().saturating_add(16);
        let memory_view = area_around_pc
            .map(|addr| {
                let pc = self.emulator.cpu().pc();
                if addr == pc {
                    format!(
                        "{:04X}: {:02X} <-- PC",
                        addr,
                        self.emulator.memory().get_u8(addr)
                    )
                } else {
                    format!("{:04X}: {:02X}", addr, self.emulator.memory().get_u8(addr))
                }
            })
            .collect::<Vec<String>>();

        let memory_paragraph = Paragraph::new(memory_view.join("\n"))
            .block(Block::default().title("Memory (PC)").borders(Borders::ALL));
        f.render_widget(memory_paragraph, memory_chunks[0]);

        // Stack view around SP
        let area_around_sp = self.emulator.cpu().sp().saturating_sub(16)
            ..self.emulator.cpu().sp().saturating_add(16);
        // Enhanced stack view showing 16-bit values
        // let stack_view = [""];
        let stack_view = area_around_sp
            .step_by(2) // Step by 2 to show 16-bit values
            .map(|addr| {
                let sp = self.emulator.cpu().sp();
                let lo = self.emulator.memory().get_u8(addr);
                let hi = self.emulator.memory().get_u8(addr.saturating_add(1));
                let value = (*lo as u16) | ((*hi as u16) << 8);

                if addr == sp {
                    format!("{:04X}: {:04X} <-- SP", addr, value)
                } else {
                    format!("{:04X}: {:04X}", addr, value)
                }
            })
            .collect::<Vec<String>>();

        let stack_paragraph = Paragraph::new(stack_view.join("\n"))
            .block(Block::default().title("Stack (SP)").borders(Borders::ALL));
        f.render_widget(stack_paragraph, memory_chunks[1]);

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
        f.render_widget(command_history_paragraph, chunks[2]);
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
                        let cmd = self.command_buffer.clone();
                        self.command_history.push(cmd.clone());
                        let mut parts = cmd.split(" ").into_iter();
                        self.command_buffer.clear();

                        match parts.next() {
                            Some("") => {
                                self.emulator.step();
                            }
                            Some("m") | Some("mem") => {
                                let addr_str = parts.next().unwrap_or("");
                                let addr = if let Some(hex) = addr_str.strip_prefix("0x") {
                                    u16::from_str_radix(hex, 16).unwrap_or(0)
                                } else {
                                    addr_str.parse::<u16>().unwrap_or(0)
                                };

                                let x = self.emulator.memory().get_u8(addr);

                                self.command_history.push(format!("> 0x{:02X}", x));
                            }
                            Some("s") | Some("step") => {
                                self.emulator.step();
                            }
                            _ => {}
                        }
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
    let args = Args::parse();

    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(&args.rom_path)?;
    let mut rom_00 = [0u8; ROM_00_SIZE];
    let bytes_read = file.read(&mut rom_00)?;
    if bytes_read < ROM_00_SIZE {
        // Optionally, you could log a warning or handle the partial fill case here.
        // The rest of rom_00 will remain zero-initialized.
    }

    let emulator = Emulator::from_rom_00(rom_00);

    let mut app = App::new(emulator);
    app.run()?;

    Ok(())
}
