# 🐾 Pet Health Insurance System 🏥

A blockchain-based pet health insurance system using NFTs to represent insurance policies with vet access for claims.

## 🌟 Features

- 🐶 Create NFT-based insurance policies for your pets
- 🩺 Veterinarian registration and verification system
- 💰 File and process insurance claims
- 🔄 Policy management (creation, cancellation)
- 🏛️ Coverage tracking and claim limits

## 📋 Contract Overview

The `pet-health` contract implements a complete pet insurance system with the following components:

- **NFT Policies**: Each insurance policy is represented as a non-fungible token
- **Vet Registry**: Verified veterinarians can file claims on behalf of pet owners
- **Claims Processing**: File, approve, and track claims against policies
- **Policy Management**: Create, view, and cancel insurance policies

## 🚀 How to Use

### For Pet Owners

1. **Create a Policy**
   ```clarity
   (contract-call? .pet-health create-policy "Fluffy" "Cat" u3 u1000000000 u50000000 u52560 u5)
   ```
   This creates a policy for a 3-year-old cat named Fluffy with 1000 STX coverage, 50 STX premium, valid for ~1 year (52560 blocks), with a maximum of 5 claims.

2. **View Your Policy**
   ```clarity
   (contract-call? .pet-health get-policy u1)
   ```

3. **Cancel a Policy**
   ```clarity
   (contract-call? .pet-health cancel-policy u1)
   ```

### For Veterinarians

1. **Register as a Vet**
   ```clarity
   (contract-call? .pet-health register-vet "Happy Paws Clinic")
   ```

2. **File a Claim**
   ```clarity
   (contract-call? .pet-health file-claim u1 u50000000 "Emergency surgery for ingested foreign object")
   ```
   This files a claim for 50 STX against policy #1.

### For Contract Owner

1. **Verify a Vet**
   ```clarity
   (contract-call? .pet-health verify-vet u1)
   ```

2. **Approve a Claim**
   ```clarity
   (contract-call? .pet-health approve-claim u1 u0)
   ```
   This approves claim #0 for policy #1.

3. **Update Claim Fee**
   ```clarity
   (contract-call? .pet-health update-claim-fee u20000000)
   ```
   This updates the claim filing fee to 20 STX.

## 💡 Implementation Details

- Policies are stored as NFTs with detailed metadata
- Claims require verification from registered veterinarians
- The contract owner must approve claims before payout
- Each policy has limits on the number of claims and total coverage

## 🔒 Security Considerations

- Only verified vets can file claims
- Only the contract owner can approve claims and verify vets
- Policy owners can only cancel their own policies
- Claims are limited by the policy's maximum claim count and coverage amount


