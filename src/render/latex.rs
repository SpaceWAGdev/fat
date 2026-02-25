pub fn escape_logic_symbols(input: &str) -> String {
    input
        .replace("^", "$∧$")
        .replace("v", "$∨$")
        .replace("<->", "$↔$")
        .replace("->", "$→$")
        .replace(">-<", "$↮$")
        .replace("!", "$¬$")
}
