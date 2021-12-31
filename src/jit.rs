use std::collections::HashMap;
use std::intrinsics::transmute;

use codegen::Context;
use memoffset::offset_of;

use cranelift::codegen::ir::SigRef;
use cranelift::prelude::types::B1;
use cranelift::prelude::*;
use cranelift::{frontend::FunctionBuilderContext, prelude::types::I64};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, ModuleError};
use peg::error::ParseError;
use peg::str::LineCol;

use crate::frontend::{parse, Operation, Register, RegisterOrLiteral};

#[derive(Debug)]
pub enum Error {
    ParseError(ParseError<LineCol>),
    CompileError(ModuleError),
}

pub struct Function {
    _module: JITModule,
    inner: fn(isize, isize, isize) -> bool,
}

impl Function {
    pub fn new(text: &str) -> Result<Self, Error> {
        match parse(text) {
            Ok(ast) => Self::compile(ast),
            Err(err) => Err(Error::ParseError(err)),
        }
    }

    pub fn compile(stmts: Vec<Operation>) -> Result<Self, Error> {
        match compile(stmts) {
            Ok((_module, inner)) => Ok(Self {
                _module,
                inner: unsafe { transmute(inner) },
            }),
            Err(err) => Err(Error::CompileError(err)),
        }
    }

    pub fn run<'a, I>(&self, it: I) -> Result<Mem, ()>
    where
        I: Iterator<Item = &'a i64> + 'a,
    {
        let mut it = NativeIterator::new(it);
        let mem = Mem::default();
        if (self.inner)(
            call_closure as *const () as isize,
            it.as_mut_ptr() as isize,
            &mem as *const _ as isize,
        ) {
            Ok(mem)
        } else {
            Err(())
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum VarKey {
    ItFunc,
    ItData,
    Mem,
    Reg(Register),
}

pub fn compile(stmts: Vec<Operation>) -> Result<(JITModule, *const u8), ModuleError> {
    let builder = JITBuilder::new(cranelift_module::default_libcall_names());
    let mut module = JITModule::new(builder);
    let mut ctx = module.make_context();

    translate(&mut module, &mut ctx, stmts);

    let id = module.declare_function("run", Linkage::Export, &ctx.func.signature)?;

    module.define_function(
        id,
        &mut ctx,
        &mut codegen::binemit::NullTrapSink {},
        &mut codegen::binemit::NullStackMapSink {},
    )?;

    module.clear_context(&mut ctx);

    module.finalize_definitions();

    let res = module.get_finalized_function(id);
    Ok((module, res))
}

fn translate(module: &mut JITModule, ctx: &mut Context, stmts: Vec<Operation>) {
    let int = I64;
    let ptr = module.target_config().pointer_type();
    let mut builder_context = FunctionBuilderContext::new();

    ctx.func.signature.params.push(AbiParam::new(ptr));
    ctx.func.signature.params.push(AbiParam::new(ptr));
    ctx.func.signature.params.push(AbiParam::new(ptr));

    ctx.func.signature.returns.push(AbiParam::new(B1));

    let error_block = ctx.func.dfg.make_block();

    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);

    builder.switch_to_block(entry_block);

    builder.seal_block(entry_block);

    let variables = declare_variables(int, ptr, &mut builder, entry_block);

    let mut signature = module.make_signature();
    signature.params.push(AbiParam::new(ptr));
    signature.returns.push(AbiParam::new(ptr));
    let signature = builder.import_signature(signature);
    for op in stmts {
        compile_op(int, error_block, signature, &variables, &mut builder, op);
    }

    return_(&variables, &mut builder, true);

    builder.insert_block_after(error_block, entry_block);
    builder.switch_to_block(error_block);
    return_(&variables, &mut builder, false);
    builder.seal_block(error_block);

    builder.finalize();
}

fn input(
    ty: Type,
    error_block: Block,
    signature: SigRef,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    reg: Register,
) {
    let it_fn = builder.use_var(variables[&VarKey::ItFunc]);
    let it_args = &[builder.use_var(variables[&VarKey::ItData])];
    let call = builder.ins().call_indirect(signature, it_fn, it_args);
    let ptr = builder.inst_results(call)[0];

    builder.ins().brz(ptr, error_block, &[]);

    let cont_block = builder.create_block();
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
    builder.seal_block(cont_block);

    let value = builder.ins().load(ty, MemFlags::trusted(), ptr, 0i32);
    builder.def_var(variables[&VarKey::Reg(reg)], value);
}

fn get_reg_or_lit(
    ty: Type,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    rol: RegisterOrLiteral,
) -> Value {
    match rol {
        RegisterOrLiteral::Register(reg) => builder.use_var(variables[&VarKey::Reg(reg)]),
        RegisterOrLiteral::Literal(n) => builder.ins().iconst(ty, n),
    }
}

fn add(
    ty: Type,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    a: Register,
    b: RegisterOrLiteral,
) {
    let var_a = variables[&VarKey::Reg(a)];
    let a = builder.use_var(var_a);
    let b = get_reg_or_lit(ty, variables, builder, b);
    let res = builder.ins().iadd(a, b);
    builder.def_var(var_a, res);
}

fn mul(
    ty: Type,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    a: Register,
    b: RegisterOrLiteral,
) {
    let var_a = variables[&VarKey::Reg(a)];
    let a = builder.use_var(var_a);
    let b = get_reg_or_lit(ty, variables, builder, b);
    let res = builder.ins().imul(a, b);
    builder.def_var(var_a, res);
}

fn div(
    ty: Type,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    a: Register,
    b: RegisterOrLiteral,
) {
    let var_a = variables[&VarKey::Reg(a)];
    let a = builder.use_var(var_a);
    let b = get_reg_or_lit(ty, variables, builder, b);
    let res = builder.ins().sdiv(a, b);
    builder.def_var(var_a, res);
}

fn rem(
    ty: Type,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    a: Register,
    b: RegisterOrLiteral,
) {
    let var_a = variables[&VarKey::Reg(a)];
    let a = builder.use_var(var_a);
    let b = get_reg_or_lit(ty, variables, builder, b);
    let res = builder.ins().srem(a, b);
    builder.def_var(var_a, res);
}

fn eql(
    ty: Type,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    a: Register,
    b: RegisterOrLiteral,
) {
    let var_a = variables[&VarKey::Reg(a)];
    let a = builder.use_var(var_a);
    let b = get_reg_or_lit(ty, variables, builder, b);
    let res = builder.ins().icmp(IntCC::Equal, a, b);
    builder.def_var(var_a, res);
}

fn compile_op(
    ty: Type,
    error_block: Block,
    signature: SigRef,
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    op: Operation,
) {
    match op {
        Operation::Inp(reg) => input(ty, error_block, signature, variables, builder, reg),
        Operation::Add(a, b) => add(ty, variables, builder, a, b),
        Operation::Mul(a, b) => mul(ty, variables, builder, a, b),
        Operation::Div(a, b) => div(ty, variables, builder, a, b),
        Operation::Mod(a, b) => rem(ty, variables, builder, a, b),
        Operation::Eql(a, b) => eql(ty, variables, builder, a, b),
    }
}

#[inline]
fn register_offset(reg: Register) -> i32 {
    (match reg {
        Register::W => offset_of!(Mem, w),
        Register::X => offset_of!(Mem, x),
        Register::Y => offset_of!(Mem, y),
        Register::Z => offset_of!(Mem, z),
    }) as i32
}

#[inline]
fn set_reg(
    variables: &HashMap<VarKey, Variable>,
    builder: &mut FunctionBuilder,
    reg: Register,
    value: Value,
) {
    let mem = builder.use_var(variables[&VarKey::Mem]);
    builder
        .ins()
        .store(MemFlags::trusted(), value, mem, register_offset(reg));
}

#[inline]
fn copy_reg(variables: &HashMap<VarKey, Variable>, builder: &mut FunctionBuilder, reg: Register) {
    let value = builder.use_var(variables[&VarKey::Reg(reg)]);
    set_reg(variables, builder, reg, value);
}

fn return_(variables: &HashMap<VarKey, Variable>, builder: &mut FunctionBuilder, what: bool) {
    for reg in [Register::W, Register::X, Register::Y, Register::Z].into_iter() {
        copy_reg(variables, builder, reg);
    }
    let res = builder.ins().bconst(B1, what);
    builder.ins().return_(&[res]);
}

struct NativeIterator<'a> {
    closure: &'a mut dyn FnMut() -> *const (),
}

impl<'a> NativeIterator<'a> {
    pub fn new<I, T>(mut it: I) -> Self
    where
        T: 'a,
        I: Iterator<Item = &'a T> + 'a,
    {
        Self {
            closure: Box::leak(Box::new(move || match it.next() {
                Some(item) => item as *const _ as *const (),
                None => std::ptr::null(),
            })),
        }
    }

    pub fn as_mut_ptr(&mut self) -> *mut () {
        (&mut self.closure) as *mut _ as *mut ()
    }
}

impl<'a> Drop for NativeIterator<'a> {
    fn drop(&mut self) {
        drop(unsafe { Box::from_raw(self.closure) })
    }
}

#[no_mangle]
unsafe extern "C" fn call_closure(f: *mut ()) -> *const ()
where
{
    let f: &mut &mut dyn FnMut() -> *const () = transmute(f);
    (&mut *f)()
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
#[repr(C, packed)]
pub struct Mem {
    w: i64,
    x: i64,
    y: i64,
    z: i64,
}

#[test]
fn test1() {
    let v: Vec<i64> = vec![1, 2, 3];
    let mut it = NativeIterator::new(v.iter());
    test1_inner(call_closure, it.as_mut_ptr());
}

#[cfg(test)]
fn test1_inner(next: unsafe extern "C" fn(*mut ()) -> *const (), data: *mut ()) {
    loop {
        let res = unsafe { next(data) };

        if res.is_null() {
            break;
        } else {
            let item = unsafe { *(res as *const i64) };
            println!("{}", item);
        }
    }
}

#[test]
fn test2() -> Result<(), Error> {
    let v: Vec<i64> = vec![1, 2, 3];
    let f = Function::new(
        "add w 2
inp x
add x 2
inp y
add y 2
inp z
add z 2",
    )?;
    println!("{:?}", f.run(v.iter()));
    Ok(())
}

fn declare_variables(
    int_ty: Type,
    ptr_ty: Type,
    builder: &mut FunctionBuilder,
    entry_block: Block,
) -> HashMap<VarKey, Variable> {
    let mut index = 0;
    let mut variables = HashMap::new();

    for (i, k) in [VarKey::ItFunc, VarKey::ItData, VarKey::Mem]
        .into_iter()
        .enumerate()
    {
        let var = Variable::new(index);
        index += 1;
        variables.insert(k, var);
        builder.declare_var(var, ptr_ty);
        builder.def_var(var, builder.block_params(entry_block)[i]);
    }

    let zero = builder.ins().iconst(int_ty, 0);
    for k in [Register::W, Register::X, Register::Y, Register::Z].into_iter() {
        let var = Variable::new(index);
        index += 1;
        variables.insert(VarKey::Reg(k), var);
        builder.declare_var(var, int_ty);
        builder.def_var(var, zero);
    }

    variables
}
