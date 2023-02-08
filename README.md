# Milestones: a Project Management Tool for Home Remodels

This is a Cardano project which enables Homeowners, Contractors, and Inspectors to easily manage major home remodel projects.

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
1. Open `nix-shell`
1. Run `cabal build`

### Creating the Inspector NFT
1. In [Inspector.Deploy.hs](src/Inspector/Deploy.hs): 
    1. Update the `inspector` field with the PubKeyHash of the wallet serving as the Inspector
    1. Update the `utxo` field with the utxo that will used to create the InspectorNFT policy
1. Run `main` in [Inspector.Deploy.hs](src/Inspector/Deploy.hs) to create `Mint.plutus`
1. Run [mintPermitFromScript.sh](scripts/mintPermitFromScript.sh) and follow the prompts to create the Inspector NFT
1. Additional scripts are provided for the other Inspector NFTs
    1. [mintRoughFromScript.sh](scripts/mintRoughFromScript.sh) - NFT to indicate `"RoughPassed"`
    1. [mintDrywallFromScript.sh](scripts/mintDrywallFromScript.sh) - NFT to indicate `"DrywallPassed"`
    1. [mintFinalFromScript.sh](scripts/mintFinalFromScript.sh) - NFT to indicate `"FinalPassed"`
    1. [mintClosedFromScript.sh](scripts/mintClosedFromScript.sh) - NFT to indicate `"ClosedIncomplete"`
        * In this scenario, the Homeowner can withdraw any funds not owed to the Contractor
1. Note that for the steps outlined in the scripts provided below, a corresponding Inspector NFT will need to be minted and selected at each step or the Milestones validation will fail

### Creating the Milestones AuthNFT
1. In [Milestones.DeployNFT.hs](src/Milestones/DeployNFT.hs): 
    1. Update the `contractor` field with the PubKeyHash of the wallet serving as the Contractor
    1. Update the `TokenName` field with a meaningful name for this Milestone project, this will be the AuthNFT's TokenName
    1. Update the `utxo` field with the utxo that will used to create the unique AuthNFT policy
1. Run `main` in [Milestones.DeployNFT.hs](src/Milestones/DeployNFT.hs) to create `Mint.plutus`
1. Run [mintMilestonesAuthNFT.sh](scripts/mintMilestonesAuthNFT.sh) and follow the prompts to create the Milestones AuthNFT
1. The AuthNFT will be sent to the Contractor's address that you provided
1. You will send this AuthNFT to the Validator's ScriptAddress using the [0-initializeContract.sh](scripts/0-initializeContract.sh) script later in this process

### Deploying the Milestones OnChain logic
1. In [Milestones.DeployOnChain.hs](src/Milestones/DeployOnChain.hs): 
    1. Update the `contractor` field with the PubKeyHash of the wallet serving as the Contractor
    1. Update the `homeowner` field with the PubKeyHash of the wallet serving as the Homeowner
    1. Update the `totalCost` field with the total cost of the project (in Lovelace)
    1. Update the individual payment fields, note that the sum of these 4 must add up to the `totalCost`
        1. Update the `deposit` field with the amount for the first Homeowner payment
        1. Update the `secondPayment` field with the amount for the second Homeowner payment
        1. Update the `thirdPayment` field with the amount for the third Homeowner payment
        1. Update the `finalPayment` field with the amount for the fourth Homeowner payment    
    1. Update the `inspectionPolicyId` field with the Inspector NFT's [policyID](src/Inspector/Deploy/policyID)
    1. Update the `projectPolicyId` field with the Milestones AuthNFT [policyID](src/Milestones/Deploy/policyID)
1. Run `main` in [Milestones.DeployOnChain.hs](src/Milestones/DeployOnChain.hs) to create `Milestones.plutus`
1. Run [0-initializeContract.sh](scripts/0-initializeContract.sh) and follow the prompts to transfer the AuthNFT from the Contractor's wallet to the Validator's ScriptAddress
1. This will transfer the AuthNFT to the Milestones ScriptAddress and set the initial `lastBalance` datum value to 2ADA (2000000 Lovelace) to account for the 2ADA MinUTXO that came with the AuthNFT
1. This will also create [milestonesScript.addr](src/Milestones/Deploy/milestonesScript.addr) which will be automatically referenced by all future shell scripts below


### Interacting with the Milestones Validator
##### You can create your own off-chain transactions, but a sequence of transactions has been created to help demonstrate the flow of the application

* Following the sequence below will demonstrate the flow of a normal project, including deliberate failing transactions that show the Validator logic preventing malicious behavior
* Several scripts have been created, prefixed `<index>-<step-name>.sh` such as [1-contractorStartProject.sh](scripts/1-contractorStartProject.sh) 
* These work with the corresponding datum files in `src/Milestones/Deploy/<index>-<step-name>` that were created with their corresponding Deploy files in `src/Milestones/DeployAdditional_<index>_<step-name>.hs`
    * You can re-run these Deploy files to create your own custom Datum files if you choose, but since they only rely on the `Milestones.plutus` and `milestonesScript.addr` that were created with your custom inputs above, you can use them as-is with no changes
    * For this walkthrough, you can ignore these Deploy and Datum files, they will be hardcoded for ease of use in the walkthrough scripts
* To use the application, simply run the scripts in order starting with [1-contractorStartProject.sh](scripts/1-contractorStartProject.sh) followed by [2-homeownerFirstDeposit.sh](scripts/2-homeownerFirstDeposit.sh) and so on
* The shell scripts will indicate what is happening at each step and what values need to be provided
* The Homeowner can deposit funds at any step of the project, except for if the Inspector NFT is `"FinalPassed"` or `"ClosedIncomplete"` since both indicate the project is over
    * If the Inspector mints the `"ClosedIncomplete"` NFT, the Homeowner will be able to remove any remaining owed funds
* Note that for each step where a contractor is withdrawing funds, the corresponding Inspector NFT needs to be minted and referenced. See the Inspector NFT scripts above to mint these as needed
    * Additionally, since each Contractor withdrawal depends on a corresponding Inspector NFT, you can easily test the expected failure case by trying to (for example) run [5-contractorWithdrawDrywall.sh](scripts/5-contractorWithdrawDrywall.sh) before the Inspector's [mintDrywallFromScript.sh](scripts/mintDrywallFromScript.sh) (or by simply selecting the wrong InspectorNFT in any of the Contractor scripts). This will result in the Milestones validator rejecting the withdrawal, since the Inspector has not passed this phase of the project. This can be checked for every Contractor step as desired
    * Step 5 in this flow is set to fail deliberately. After confirming this failure, you can continue with [6-contractorWithdrawRough.sh](scripts/6-contractorWithdrawRough.sh)
* When you reach step 8 in the provided workflow, you can choose to either have the Contractor get their final payment with [8a-contractorWithdrawFinal.sh](scripts/8a-contractorWithdrawFinal.sh), or if you mint a `"ClosedIncomplete"` NFT from the Inspector using [mintClosedFromScript.sh](scripts/mintClosedFromScript.sh), you can have the Homeowner retrieve their balance of funds using [8b-homeownerWithdrawProjectFail.sh](scripts/8b-homeownerWithdrawProjectFail.sh)