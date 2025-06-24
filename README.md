# 🌍 Lingochain - Local Language Preservation DAO

A decentralized autonomous organization (DAO) built on Stacks blockchain for funding and supporting local language preservation efforts worldwide.

## 📋 Overview

Lingochain enables communities to collectively fund language preservation projects including:
- 🗣️ Supporting native speakers
- 📚 Creating dictionaries and educational materials  
- 🎵 Recording traditional songs and stories
- 📖 Documenting endangered languages

## ✨ Features

- **DAO Membership**: Join by contributing STX tokens
- **Proposal System**: Create funding proposals for language projects
- **Democratic Voting**: Members vote on proposals with quorum requirements
- **Treasury Management**: Secure fund management and distribution
- **Project Registry**: Track language preservation projects and their funding status

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet with STX tokens

### Deployment

```bash
clarinet deploy
```

## 📖 Usage

### Joining the DAO

```clarity
(contract-call? .Lingochain join-dao u1000000) ;; Contribute 1 STX
```

### Creating a Proposal

```clarity
(contract-call? .Lingochain create-proposal 
    "Maori Dictionary Project" 
    "Fund creation of comprehensive Maori-English dictionary"
    u5000000 
    'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7
    "dictionary")
```

### Voting on Proposals

```clarity
(contract-call? .Lingochain vote-on-proposal u1 true) ;; Vote yes on proposal 1
```

### Registering Language Projects

```clarity
(contract-call? .Lingochain register-language-project
    "Cherokee"
    "North America"
    u2000
    "recording")
```

## 🏛️ Governance

- **Voting Period**: 144 blocks (~24 hours)
- **Quorum**: 51% approval required
- **Minimum Participation**: 1/3 of members must vote
- **Minimum Proposal**: 1 STX

## 📊 Contract Functions

### Public Functions
- `join-dao(contribution)` - Become a DAO member
- `create-proposal(...)` - Submit funding proposal
- `vote-on-proposal(id, vote)` - Vote on proposals
- `execute-proposal(id)` - Execute passed proposals
- `register-language-project(...)` - Register new language project
- `contribute-to-treasury(amount)` - Add funds to treasury

### Read-Only Functions
- `get-proposal(id)` - Get proposal details
- `get-treasury-balance()` - Check DAO treasury
- `is-member(user)` - Check membership status
- `proposal-passed?(id)` - Check if proposal passed
- `get-language-project(id)` - Get project details

## 🔒 Security Features

- Member-only proposal creation and voting
- Quorum requirements for proposal passage
- Time-locked voting periods
- Treasury balance validation
- Double-voting prevention

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details

---

*Preserving languages, one block at a time* 🌐⛓️

