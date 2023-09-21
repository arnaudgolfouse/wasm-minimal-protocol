use std::collections::{HashMap, HashSet};

use wast::{
    core::{
        Expression, Func, FuncKind, InlineExport, Instruction, ItemKind, Local, ModuleField,
        ModuleKind, TypeUse,
    },
    token::Index,
    Wat,
};

pub enum FunctionsToStub {
    All,
    Some(HashSet<String>),
}
pub struct ShouldStub {
    pub modules: HashMap<String, FunctionsToStub>,
}
impl Default for ShouldStub {
    fn default() -> Self {
        Self {
            modules: [(String::from("wasi_snapshot_preview1"), FunctionsToStub::All)]
                .into_iter()
                .collect(),
        }
    }
}

enum ImportIndex {
    ToStub(u32),
    Keep(u32),
}

impl ShouldStub {
    fn should_stub(&self, module: &str, function: &str) -> bool {
        if let Some(functions) = self.modules.get(module) {
            match functions {
                FunctionsToStub::All => true,
                FunctionsToStub::Some(functions) => functions.contains(function),
            }
        } else {
            false
        }
    }
}

pub fn stub_wasi_functions(
    binary: &[u8],
    should_stub: ShouldStub,
    return_value: u32,
) -> anyhow::Result<Vec<u8>> {
    let wat = wasmprinter::print_bytes(binary)?;
    let parse_buffer = wast::parser::ParseBuffer::new(&wat)?;

    let mut wat: Wat = wast::parser::parse(&parse_buffer)?;
    let module = match &mut wat {
        Wat::Module(m) => m,
        Wat::Component(_) => {
            anyhow::bail!("components are not supported")
        }
    };
    let fields = match &mut module.kind {
        ModuleKind::Text(f) => f,
        ModuleKind::Binary(_) => {
            anyhow::bail!("binary directives are not supported");
        }
    };

    let mut types = Vec::new();
    let mut nb_imports: u32 = 0;
    let mut to_stub = Vec::new();
    let mut insert_stubs_index = None;
    let mut new_import_indices = Vec::new();

    for (field_idx, field) in fields.iter_mut().enumerate() {
        match field {
            ModuleField::Type(t) => types.push(t),
            ModuleField::Import(i) => {
                let typ = match &i.item.kind {
                    ItemKind::Func(typ) => typ.index.and_then(|index| match index {
                        Index::Num(index, _) => Some(index as usize),
                        Index::Id(_) => None,
                    }),
                    _ => None,
                };
                let new_index = match typ {
                    Some(type_index) if should_stub.should_stub(i.module, i.field) => {
                        println!("Stubbing function {}::{}", i.module, i.field);
                        let typ = &types[type_index];
                        let ty = TypeUse::new_with_index(Index::Num(type_index as u32, typ.span));
                        let wast::core::TypeDef::Func(func_typ) = &typ.def else {
                            continue;
                        };
                        let locals: Vec<Local> = func_typ
                            .params
                            .iter()
                            .map(|&(id, name, ty)| Local { id, name, ty })
                            .collect();
                        let nb_results = func_typ.results.len();
                        let span = i.span;
                        let name = i.item.name;
                        let instructions = {
                            let mut res = Vec::with_capacity(nb_results);
                            for _ in 0..nb_results {
                                res.push(Instruction::I32Const(return_value as i32));
                            }
                            res
                        };
                        *field = ModuleField::Func(Func {
                            span,
                            id: i.item.id,
                            name,
                            // no exports
                            exports: InlineExport { names: Vec::new() },
                            kind: wast::core::FuncKind::Inline {
                                locals: locals.into_boxed_slice(),
                                expression: Expression {
                                    instrs: instructions.into_boxed_slice(),
                                },
                            },
                            ty,
                        });
                        to_stub.push(field_idx);
                        ImportIndex::ToStub(to_stub.len() as u32 - 1)
                    }
                    _ => {
                        nb_imports += 1;
                        ImportIndex::Keep(nb_imports - 1)
                    }
                };
                new_import_indices.push(new_index);
            }
            ModuleField::Func(func) => {
                if insert_stubs_index.is_none() {
                    insert_stubs_index = Some(field_idx);
                }
                match &mut func.kind {
                    FuncKind::Import(f) => {
                        if should_stub.should_stub(f.module, f.field) {
                            println!("[WARNING] Stubbing inline function is not yet supported");
                            println!(
                                "[WARNING] ignoring inline function \"{}\" \"{}\"",
                                f.module, f.field
                            );
                        }
                    }
                    FuncKind::Inline { expression, .. } => {
                        for inst in expression.instrs.as_mut().iter_mut() {
                            match inst {
                                Instruction::RefFunc(Index::Num(index, _))
                                | Instruction::ReturnCall(Index::Num(index, _))
                                | Instruction::Call(Index::Num(index, _)) => {
                                    if let Some(new_index) = new_import_indices.get(*index as usize)
                                    {
                                        *index = match new_index {
                                            ImportIndex::ToStub(idx) => *idx + nb_imports,
                                            ImportIndex::Keep(idx) => *idx,
                                        };
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    let insert_stubs_index = insert_stubs_index
        .expect("This is weird: there are no code sections in this wasm executable !");

    for (already_stubbed, fields_index) in to_stub.into_iter().enumerate() {
        // Put the function at fields_index to insert_stubs_index.
        let func = fields.remove(fields_index - already_stubbed);
        debug_assert!(matches!(func, ModuleField::Func(_)));
        fields.insert(insert_stubs_index - 1, func);
    }

    Ok(module.encode()?)
}
