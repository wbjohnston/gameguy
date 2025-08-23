use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    rom_path: String,
}

fn main() {
    let args = Args::parse();
    dbg!(&args);
}
