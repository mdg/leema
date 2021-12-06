use crate::leema::failure::Lresult;
use crate::leema::reg::Reg;
use crate::leema::val::Val;

/// StackBuffer stores the data for a particular fiber/task
#[derive(Debug)]
pub struct Buffer
{
    data: Vec<Val>,
}

impl Buffer
{
    pub fn new(size: usize) -> Buffer
    {
        Buffer {
            data: Vec::with_capacity(size),
        }
    }

    pub fn frame(&mut self) -> Frame
    {
        self.data.push(Val::VOID);
        let func = self.data.last().unwrap();

        Frame {
            result: Val::VOID,
            func,
            args: &[],
            vars: &mut [],
            stack: self,
        }
    }
}

#[derive(Debug)]
pub struct Frame
{
    result: Val,
    // result_ptr: Option<*mut Val>,
    // subj: Option<*mut Val>,
    func: *const Val,
    args: *const [Val],
    vars: *mut [Val],
    stack: *mut Buffer,
}

impl Frame
{
    pub fn push_call(&mut self, _result: Reg, _call: Reg) // -> Lresult<Frame>
    {
    }

    pub fn tail_call(&mut self, _call: Reg) // -> Lresult<Frame>
    {
    }

    pub fn get(&self, i: usize) -> Lresult<&Val>
    {
        unsafe { Ok((*self.stack).data.get(i).unwrap()) }
    }

    pub fn get_mut(&mut self, i: usize) -> Lresult<&mut Val>
    {
        unsafe { Ok((*self.stack).data.get_mut(i).unwrap()) }
    }

    pub fn set(&mut self, i: usize, d: Val) -> Lresult<()>
    {
        unsafe {
            *(*self.stack).data.get_mut(i).unwrap() = d;
        }
        Ok(())
    }
}
