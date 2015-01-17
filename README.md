<!-- -*- mode: markdown; coding: utf-8 -*- -->

# StratumTool – Bitcoin network query tool

This tool allows querying Electrum servers using Stratum Protocol
without local Electrum installation. Possible use cases include
querying bitcoin address balances, block headers, raw transactions,
unspent outputs, or any other command supported by Electrum protocol.

The internal implementation is more complete than the command-line
interface so it is easy to expand to more complex queries in future.

## Useful commands

Here is listing of useful commands. Of course you may use any other
command supported by
[Stratum protocol](https://electrum.orain.org/wiki/Stratum_protocol_specification).

Command | Arguments | Synopsis
------- | --------- | --------
`blockchain.address.get_history` | Bitcoin address | Lists transactions with TxOuts to given address
`blockchain.address.get_balance` | Bitcoin address | Lists both unconfirmed and confirmed balances of given address
`blockchain.address.listunspent` | Bitcoin address | List unspent transactions of given address
`blockchain.utxo.get_address` | Transaction hash and index | Get target address of given transaction hash output
`blockchain.transaction.broadcast` | Hex encoded transaction | Broadcast given transaction
`blockchain.transaction.get` | Transaction hash | Retrieves given transaction in hex encoded format
`blockchain.block.get_header` | Block hash | Retrieves given block headers

All command with only single argument may be used in `--multi` mode
which allows running the command with multiple data on single
invocation.

## Examples

Querying Kryptoradio wallet balance:

	stratum-tool blockchain.address.get_balance 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5

Query multiple wallets at once

	stratum-tool -m blockchain.address.get_balance 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5 1duckuPMxPbfDvx2HBBtGHataWtqb9X1Z

Single command with multiple arguments:

	stratum-tool blockchain.utxo.get_address 7e791721c61415d26966ab8530ffd550a045ad1b79ca870a4eca14f57b6a5b8c 1

For more information, see `stratum-tool --help`

## Output format

The output depends on output formatting and Stratum command. By default the output format is JSON in breadcrumbs format which is useful for shell scripts and humans:

	$ stratum-tool -m blockchain.address.get_balance 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5 1duckuPMxPbfDvx2HBBtGHataWtqb9X1Z
	1duckuPMxPbfDvx2HBBtGHataWtqb9X1Z.unconfirmed = 0
	1duckuPMxPbfDvx2HBBtGHataWtqb9X1Z.confirmed = 2317254
	1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5.unconfirmed = 0
	1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5.confirmed = 71286480

When using `--json` option the output format is JSON:

	$ stratum-tool -r -m blockchain.address.get_balance 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5 1duckuPMxPbfDvx2HBBtGHataWtqb9X1Z
	{"1duckuPMxPbfDvx2HBBtGHataWtqb9X1Z":{"unconfirmed":0,"confirmed":2317254},"1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5":{"unconfirmed":0,"confirmed":71286480}}

## Compiling

StratumTool is written in [Haskell](http://en.wikipedia.org/wiki/Haskell_%28programming_language%29) and can be built using Cabal:

If you are running Debian or Ubuntu then you can install the
dependencies using *apt* first which make the installation faster and also more
robust (we like to avoid nasty dependency hell).

	sudo apt-get install cabal-install ghc libghc-aeson-dev libghc-stm-dev libghc-cmdargs-dev libghc-network-dev

The following libraries may be fetched over Cabal so it's not fatal if
you don't have them but if your distribution has them then it's better
to use them, too:

	sudo apt get install libghc-async-dev

Then install via cabal

	cabal update
	cabal install

The binaries will be placed at `~/.cabal/bin`

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Contact

Bug reports and feature requests are welcome via
[GitHub issues](https://github.com/zouppen/stratum-tool/issues). You
can also contact the author via e-mail address joel.lehtonen@koodilehto.fi.

If you find this product useful you can show your support to
Kryptoradio project (`1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5`) or buy some
stuff from [BTC Store](https://btcstore.eu/). :-)
