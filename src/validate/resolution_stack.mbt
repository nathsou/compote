
enum ResolutionStackEntry {
  Loop(String)
  Switch(String)
}

pub struct ResolutionStack(Array[ResolutionStackEntry])

pub fn ResolutionStack::new() -> ResolutionStack {
  ResolutionStack([])
}

pub fn ResolutionStack::push(self: ResolutionStack, entry: ResolutionStackEntry) -> Unit {
  self.0.push(entry)
}

pub fn ResolutionStack::pop(self: ResolutionStack) -> Unit {
  let _ = self.0.pop()
}

pub fn ResolutionStack::peek(self: ResolutionStack) -> ResolutionStackEntry? {
  self.0.last()
}

pub fn ResolutionStack::get_innermost_loop_label(self: ResolutionStack) -> String? {
  for i = self.0.length() - 1; i >= 0; i = i - 1 {
    if self.0[i] is Loop(label) {
      return Some(label)
    }
  }

  None
}
