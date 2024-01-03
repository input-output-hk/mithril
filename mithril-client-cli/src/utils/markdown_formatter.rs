

pub fn format_table(header: &Vec<&str>, lines: &Vec<Vec<String>>) -> String {
    format!("{}\n{}",
        format_table_header(header),
        lines.iter().map(|line| format_table_line(line)).collect::<Vec<String>>().join("\n"),
    )
}

pub fn format_table_line(data: &Vec<String>) -> String {
    format!("| {} |", data.join(" | "))
}

pub fn format_table_header(data: &Vec<&str>) -> String {
    let headers = data.iter().map(|header| {
        let align_left = header.chars().next().map(|c| c == ':').unwrap_or(false);
        let align_right = header.chars().last().map(|c| c == ':').unwrap_or(false);
        let label = &header[(if align_left {1} else {0})..(header.len()-(if align_right {1} else {0}))];
        (label, align_left, align_right)
    }).collect::<Vec<(&str, bool, bool)>>();

    let sublines = headers.iter().map(|(label, left, right)| {
        format!("{}{}{}", if *left {":"} else {"-"}, "-".repeat(label.len()), if *right {":"} else {"-"})
    }).collect::<Vec<String>>();

    let labels = headers.iter().map(|(label, _, _)| {
        label.to_string()
    }).collect::<Vec<String>>();

    format!("| {} |\n|{}|", labels.join(" | "), sublines.join("|"))
}
