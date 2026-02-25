pub fn escape_logic_symbols(input: &str) -> String {
    input
        .replace("^", "$∧$")
        .replace("^", "$∨$")
        .replace("->", "$->$")
        .replace("<->", "$↔$")
        .replace(">-<", "$↮$")
        .replace("!", "$¬$")
}
