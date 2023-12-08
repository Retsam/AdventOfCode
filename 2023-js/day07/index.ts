import fs from "fs";
import _ from "lodash";

const input = fs.readFileSync(process.stdin.fd).toString().trim();

const lines = input.split("\n");

const data = lines.map((x) => {
  const [cards, bid] = x.split(" ");
  return [cards.split(""), parseInt(bid)] as const;
});

const handType = (hand: string[], includeJokers: boolean): number => {
  const counts = Object.values(
    _.countBy(hand.filter((c) => !(includeJokers && c === "J")))
  );
  const jokerCount = includeJokers ? hand.filter((c) => c === "J").length : 0;

  const fiveKind = counts.includes(5);
  const fourKind = counts.includes(4);
  const threeKind = counts.includes(3);
  const pairCount = counts.filter((c) => c === 2).length;

  if (
    fiveKind ||
    (fourKind && jokerCount === 1) ||
    (threeKind && jokerCount === 2) ||
    (pairCount && jokerCount === 3) ||
    jokerCount >= 4
  ) {
    return 6;
  }
  if (
    fourKind ||
    (threeKind && jokerCount === 1) ||
    (pairCount && jokerCount === 2) ||
    jokerCount === 3
  ) {
    return 5;
  }
  if ((threeKind && pairCount == 1) || (pairCount === 2 && jokerCount === 1)) {
    return 4;
  }
  if (threeKind || (pairCount && jokerCount === 1) || jokerCount === 2) {
    return 3;
  }
  if (pairCount == 2) {
    return 2;
  }
  if (pairCount == 1 || jokerCount === 1) {
    return 1;
  }
  return 0;
};

const scoreHand = (hand: string[], includeJokers: boolean): number[] => {
  // Card rankings, used as "tiebreak"
  const cardRanks = includeJokers ? "J23456789TQKA" : "23456789TJQKA";
  const ranks = hand.map((card) => cardRanks.indexOf(card));
  return [handType(hand, includeJokers), ...ranks];
};

const solve = (includeJokers: boolean) => {
  return _(data)
    .sortBy(([hand]) =>
      // .sortBy is going to sort by strings, so left pad so it's consistent
      scoreHand(hand, includeJokers).map((v) => v.toString().padStart(2, "0"))
    )
    .map(([, bid], i) => bid * (i + 1))
    .sum();
};

console.log(solve(false));
console.log(solve(true));
