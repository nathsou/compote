pub struct Scope[T] {
  members: Map[String, T]
  parent: Scope[T]?
}

pub suberror ScopeError {
  MemberAlreadyDeclared
}

pub fn[T] Scope::new() -> Scope[T] {
  { members: {}, parent: None }
}

pub fn[T] Scope::child(self: Scope[T]) -> Scope[T] {
  { members: {}, parent: Some(self) }
}

pub fn[T] Scope::add(self: Scope[T], name: String, value: T) -> Unit raise ScopeError {
  if self.members.contains(name) {
    raise MemberAlreadyDeclared
  }

  self.members[name] = value
}

pub fn[T] Scope::add_shadowing(self: Scope[T], name: String, value: T) -> Unit {
  self.members[name] = value
}

pub fn[T] Scope::contains(self: Scope[T], name: String) -> Bool {
  self.members.contains(name) || (match self.parent {
    Some(parent) => parent.contains(name)
    None => false
  })
}

pub fn[T] Scope::contains_local(self: Scope[T], name: String) -> Bool {
  self.members.contains(name)
}

pub fn[T] Scope::resolve(self: Scope[T], name: String) -> T? {
  match self.members.get(name) {
    Some(value) => Some(value)
    None => match self.parent {
      Some(parent) => parent.resolve(name)
      None => None
    }
  }
}
