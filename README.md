# Supply Chain Transparency (SCT) Smart Contract

A Clarity smart contract for tracking fair trade goods through the supply chain using NFTs.

## Features

- Create batch NFTs for product tracking
- Record supply chain stages with location and quality checks
- Fair trade certification system
- Full transparency of product journey
- Ownership transfer capabilities

## Contract Functions

### Core Functions

- `create-batch`: Create a new batch NFT with product details
- `update-stage`: Record a new stage in the supply chain
- `certify-fair-trade`: Mark a batch as fair trade certified
- `transfer`: Transfer batch ownership

### Query Functions

- `get-batch-details`: Get complete batch information
- `get-stage-details`: Get specific stage details
- `get-all-stages`: Get summary of all stages
- `is-fair-trade-certified`: Check certification status
- `get-owner`: Get current batch owner

## Usage

1. Deploy contract to Stacks blockchain
2. Create new batch using `create-batch`
3. Update stages as product moves through supply chain
4. Query product journey using read functions
5. Certify products as fair trade when requirements are met

## Requirements

- Clarinet
- Stacks blockchain wallet
- Required permissions for contract interactions
```
