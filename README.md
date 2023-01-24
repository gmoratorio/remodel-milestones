# Milestones: a Project Management Tool for Home Remodels

This is a Cardano project which enables homeowners, contractors, and inspectors to easily manage major home remodel projects.

## Workflow

This project utilizes Cardano Smart Contracts to simplify the process of scheduled payments for major milestones in a project. The high-level workflow is below:

1. Once the Homeowner and Contractor have signed a contract, the Contractor will pull a permit with an Inspector (usually a county or other municipality)
1. Once the permit is issued, the Inspector creates a unique NFT Policy (using [mintPermitFromScript.sh](scripts/mintPermitFromScript.sh))
1. The Homeowner can then deposit funds into the contract
1. The Contractor will be able to retrieve funds from the contract as milestones are met and published to the unique NFT Policy
1. If the project is `ClosedIncomplete`, the Homeowner will be able to retrieve their remaining funds

## Technical Workflow

To create the NFT project for the milestones:

1. Clone this repository
1. In [Deploy.hs](src/Inspector/Deploy.hs): 
    1. Update the `inspector` field with the PubKeyHash of the wallet serving as the Inspector
    1. Update the `utxo` field with the utxo that will used to create the policy
1. Open `nix-shell`
1. Run `cabal build`
1. Run `main` in `Deploy.hs` to create `Mint.plutus`
1. Run `mintPermitFromScript.sh` and follow the prompts to create the NFT milestone project
1. Execute the following NFT scripts as each milestone is met
