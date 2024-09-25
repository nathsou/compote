
pub struct ResolutionStackSwitchEntry {
  label: String
  cases: @hashset.T[@parse.Constant]
  mut has_default: Bool
}

pub fn ResolutionStackSwitchEntry::new(label: String) -> ResolutionStackSwitchEntry {
  { label, cases: @hashset.new(), has_default: false }
}

pub enum ResolutionStackEntry {
  Loop(String)
  Switch(ResolutionStackSwitchEntry)
}

pub type ResolutionStack Array[ResolutionStackEntry]

pub fn ResolutionStack::new() -> ResolutionStack {
  []
}

pub fn ResolutionStack::push(self: ResolutionStack, entry: ResolutionStackEntry) -> Unit {
  self._.push(entry)
}

pub fn ResolutionStack::pop(self: ResolutionStack) -> Unit {
  let _ = self._.pop()
}

pub fn ResolutionStack::peek(self: ResolutionStack) -> ResolutionStackEntry? {
  self._.last()
}

pub fn ResolutionStack::get_innermost_loop_label(self: ResolutionStack) -> String? {
  for i = self._.length() - 1; i >= 0; i = i -1 {
    match self._[i] {
      Loop(label) => return Some(label)
      _ => ()
    }
  }

  None
}

pub fn ResolutionStack::get_innermost_switch_entry(self: ResolutionStack) -> ResolutionStackSwitchEntry? {
  for i = self._.length() - 1; i >= 0; i = i - 1 {
    match self._[i] {
      Switch(entry) => return Some(entry)
      _ => ()
    }
  }

  None
}
