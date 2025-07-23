# GraviDEX 🌌

## Gravity-Powered Decentralized Exchange

GraviDEX is a native Stacks Automated Market Maker (AMM) that enables seamless token swaps and liquidity pooling using the proven constant product formula (x*y=k).

## 🚀 Features

- **Token Swaps**: Swap STX and SIP-010 tokens with minimal slippage
- **Liquidity Pools**: Provide liquidity and earn fees from trading activity  
- **LP Token System**: Receive LP tokens representing your pool share
- **Slippage Protection**: Built-in slippage controls for safe trading
- **Fee Management**: Configurable trading fees with transparent fee structure
- **Pool Analytics**: Track volume, reserves, and accumulated fees

## 🛠 Technical Implementation

- **Smart Contract**: Written in Clarity for security and predictability
- **AMM Model**: Implements constant product formula (x*y=k)
- **Fee Structure**: 0.3% trading fee distributed to liquidity providers
- **Security**: Comprehensive input validation and error handling
- **Gas Optimization**: Efficient contract functions to minimize transaction costs

## 📋 Contract Functions

### Core Trading Functions
- `swap-exact-tokens-for-tokens` - Execute token swaps with slippage protection
- `create-pool` - Initialize new trading pairs
- `add-liquidity` - Provide liquidity to existing pools
- `remove-liquidity` - Withdraw liquidity and claim fees

### Read-Only Functions  
- `get-pool-info` - Retrieve pool reserves and statistics
- `calculate-swap-quote` - Preview swap amounts before execution
- `get-user-liquidity` - Check user's LP token balance

## 🔧 Usage

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Testing
```bash
clarinet check
clarinet test
```

### Deployment
```bash
clarinet deploy --testnet
```

## 🤝 Contributing

Contributions are welcome! Please ensure all code passes `clarinet check` before submitting PRs.

## 🔮 Roadmap

See [Future Upgrades](#future-upgrades) section for planned enhancements including multi-token pools, dynamic fees, and advanced analytics.

*Built with ❤️ for the Stacks ecosystem*