use std::collections::HashMap;

use clap::{Command, Arg, builder::StyledStr};

use super::{StructDoc, FieldDoc};

/// Extract information of an command line argument.
fn extract_arg(arg: &Arg) -> FieldDoc {
    let parameter = arg.get_id().to_string();
    let short_option = arg.get_short().map_or("".into(), |c| format!("-{}", c));
    let long_option = arg.get_long().map_or("".into(), |c| format!("--{}", c));
    //let env_variable = arg.get_env().and_then(OsStr::to_str).map_or("".into(), |s| format!("`{}`", s));       
    let env_variable = "???".to_string();
    let description = arg.get_help().map_or("".into(), StyledStr::to_string);
    let default_value = if arg.get_default_values().iter().count() == 0 {
        None
    } else {
        Some(arg.get_default_values().iter().map(|s| format!("{}", s.to_string_lossy())).collect::<Vec<String>>().join(","))
    };
    let example = String::from("?");
    
    FieldDoc {
        parameter: parameter,
        command_line_long: long_option,
        command_line_short: short_option,
        environment_variable: env_variable,
        description: description,
        default_value: default_value,
        example: example,
        is_mandatory: arg.is_required_set(),
    }
}

pub fn extract_parameters(cmd: &Command) -> StructDoc {
    StructDoc {
        data: cmd.get_arguments().map(extract_arg).collect::<Vec<FieldDoc>>(),
    }
}

pub fn merge_struct_doc(s1: &StructDoc, s2: &StructDoc) -> StructDoc {
   
    let mut data_map1 = s1.data.iter()
        .map(|field_doc| (field_doc.parameter.clone(), field_doc.clone()))
        .collect::<HashMap<_,_>>();

    for field_doc in s2.data.iter() {
        if !data_map1.contains_key(&field_doc.parameter) {
            data_map1.insert(field_doc.parameter.clone(), field_doc.clone());
        } else {
           
            let mut d = data_map1.get(&field_doc.parameter).unwrap().clone();
            if d.default_value.is_none() {
                d.default_value = field_doc.default_value.clone();
                data_map1.insert(field_doc.parameter.clone(), d);
            }
        }
    }
    let result = StructDoc {
        data: data_map1.values().cloned().collect(),
    };
    result
}

#[cfg(test)]
mod tests {

    use clap::{Parser, CommandFactory};
    use super::*;

    #[derive(Parser, Debug, Clone)]
    #[command(version)]
    pub struct MyCommand {
        /// Run Mode
        #[clap(short, long, default_value = "dev")]
        run_mode: String,

        #[clap()]
        param_without_default: String,
    }
    
    #[test]
    fn test_extract_arg_info() {
        let command = MyCommand::command();
        let arg = command.get_arguments().next().unwrap();
        let parameter: FieldDoc = extract_arg(&arg);

        assert_eq!("run_mode", parameter.parameter);
        assert_eq!("-r".to_string(), parameter.command_line_short);
        assert_eq!("--run-mode".to_string(), parameter.command_line_long);
        assert_eq!(Some("dev".to_string()), parameter.default_value);
        assert_eq!("Run Mode".to_string(), parameter.description);
        //assert_eq!("???".to_string(), parameter.example);
        //assert_eq!(false, parameter.is_required);
        assert_eq!(false, parameter.is_mandatory);
    }

    #[test]
    fn test_extract_required_arg() {
        let command = MyCommand::command();
        let arg = command.get_arguments().filter(|arg| arg.get_id().to_string() == "param_without_default").next().unwrap();
        let parameter: FieldDoc = extract_arg(&arg);

        assert_eq!(true, parameter.is_mandatory);
    }

    #[test]
    fn test_extract_command_info() {
        let command = MyCommand::command();
        let command_parameters:StructDoc = extract_parameters(&command);
        
        //assert_eq!(1, command_parameters.data.len());
        assert_eq!("run_mode", command_parameters.data.first().unwrap().parameter);
        for arg in command.get_arguments() {
            println!("{} {}", arg.get_id(), arg.is_required_set());
        }
    }

    #[test]
    fn test_merge_struct_doc() {
        let s1 = {
            let mut s = StructDoc::default();
            s.add_param("A", "Param first A", Some("default A".to_string()));
            s.add_param("B", "Param first B", None);
            s.add_param("C", "Param first C", Some("default C".to_string()));
            s.add_param("D", "Param first D", None);
            s
        };

        let s2 = {
            let mut s = StructDoc::default();
            s.add_param("A", "Param second A", None);
            s.add_param("B", "Param second B", Some("default B".to_string()));
            s.add_param("E", "Param second E", None);
            s.add_param("F", "Param second F", Some("default F".to_string()));
            s
        };

        let result = merge_struct_doc(&s1, &s2);

        let data = result.data;
        let data_map = data.into_iter().map(|fieldDoc| (fieldDoc.parameter.clone(), fieldDoc)).collect::<HashMap<_,_>>();

        assert_eq!(6, data_map.iter().count());
        assert_eq!("Param first A", data_map.get("A").unwrap().description);
        assert_eq!("Param first B", data_map.get("B").unwrap().description);
        assert_eq!("Param first C", data_map.get("C").unwrap().description);
        assert_eq!("Param first D", data_map.get("D").unwrap().description);
        assert_eq!("Param second E", data_map.get("E").unwrap().description);
        assert_eq!("Param second F", data_map.get("F").unwrap().description);

        assert_eq!(Some("default A".to_string()), data_map.get("A").unwrap().default_value);
        assert_eq!(Some("default B".to_string()), data_map.get("B").unwrap().default_value);
        assert_eq!(Some("default C".to_string()), data_map.get("C").unwrap().default_value);
        assert_eq!(None, data_map.get("D").unwrap().default_value);
        assert_eq!(None, data_map.get("E").unwrap().default_value);
        assert_eq!(Some("default F".to_string()), data_map.get("F").unwrap().default_value);

    }
}