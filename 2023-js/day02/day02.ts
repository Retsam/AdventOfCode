import fs from "fs";

const file = fs.readFileSync("./input.txt");
// const file = fs.readFileSync("./example.txt");

const input = file.toString().trim();

type Color = "red" | "green" | "blue";

const games = input.split("\n").map((line) => {
  const [gid, rest] = line.split(": ");
  const gameId = parseInt(gid.slice("Game ".length));
  const moves = rest.split("; ").flatMap((round) =>
    round.split(", ").map((move) => {
      const [num, color] = move.split(" ");
      return [parseInt(num), color as Color] as const;
    })
  );
  return [gameId, moves] as const;
});

const maxes = {
  red: 12,
  green: 13,
  blue: 14,
};

const part1 = games
  .filter(([_, moves]) => moves.every(([num, color]) => num <= maxes[color]))
  .map(([i]) => i)
  .reduce((a, b) => a + b);

const part2 = games
  .map(([_, moves]) => {
    const mins = { red: 0, green: 0, blue: 0 };
    moves.forEach(([num, color]) => {
      mins[color] = Math.max(num, mins[color]);
    });
    return mins.red * mins.green * mins.blue;
  })
  .reduce((a, b) => a + b);

console.log(part1, part2);
