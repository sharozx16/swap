 import { Blockfrost, Lucid } from "https://deno.land/x/lucid@0.7.0/mod.ts"; Deno


const lucid = await Lucid.new(
  new Blockfrost("https://cardano-testnet.blockfrost.io/api/v0", "testnetp0gcgAf8Q1qW1F7xZuAAlRLdRwg02u6D"),
  "Testnet",
);

// Assumes you are in a browser environment
const api = await window.cardano.nami.enable();
lucid.selectWallet(api);

const tx = await lucid.newTx()
  .payToAddress("addr...", { lovelace: 5000000n })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();

console.log(txHash);