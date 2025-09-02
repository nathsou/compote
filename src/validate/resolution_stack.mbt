
enum ResolutionStackEntry {
  Loop(String)
  Switch(String)
}

pub struct ResolutionStack(Array[ResolutionStackEntry])

pub fn ResolutionStack::new() -> ResolutionStack {
  ResolutionStack([])
}

pub fn ResolutionStack::push(self: ResolutionStack, entry: ResolutionStackEntry) -> Unit {
  self.inner().push(entry)
}

pub fn ResolutionStack::pop(self: ResolutionStack) -> Unit {
  let _ = self.inner().pop()
}

pub fn ResolutionStack::peek(self: ResolutionStack) -> ResolutionStackEntry? {
  self.inner().last()
}

pub fn ResolutionStack::get_innermost_loop_label(self: ResolutionStack) -> String? {
  for i = self.inner().length() - 1; i >= 0; i = i - 1 {
    if self.inner()[i] is Loop(label) {
      return Some(label)
    }
  }

  None
}
