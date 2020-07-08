enum Role {Viewer, Approver, SigningParty}

sig User {}

sig Document {
  signatories: some User,
  roles: signatories -> one Role
}

sig Requirement {
  user: User,
  document: Document,
  requires: Role
}

one sig Flow {
  users: some User,
  documents: set Document,
  requirements: set Requirement
}{
  requirements.document = documents
  users = documents.signatories
  requirements.user = documents.signatories
  all r: Requirement |
    r.requires = r.user.(r.document.roles)
  all d: Document |
    all s: d.signatories |
      some r: requirements {
        r.user = s
        r.document = d
        r.requires = s.(d.roles)
      }
}

pred example {
  #Flow.documents >= 2
  all d: Document {
    #d.signatories >= 1
  }
}

run example for 5