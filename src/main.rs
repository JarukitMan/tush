use tush::lit;

fn main() {
    println!("{} {} {} {} {}",
        lit::Lit::Int(10),
        lit::Lit::Flt(10.0),
        lit::Lit::Bln(true),
        lit::Lit::Chr('c'),
        lit::Lit::Pth(
            std::path::Path::new(".")
            .canonicalize()
            .expect("No loopback link? Seriously?")
            .into()
        )
    );
}
