import { Dir } from "./dir.js";
import { Branded, range } from "./utils.js";

// The non-branded type: accepted internally in places where the offset matters and the memoization isn't used
type RawCoord = [number, number];
export type Coord = Branded<RawCoord, "Coord">;

const _coordCache = new Map<string, Coord>();
let [minX, minY, maxX, maxY] = [0, 0, 0, 0];

// Memoizing constructor: this lets us use `===` to compare coordinates and use them as Set/Map keys
export const xy = (x: number, y: number): Coord => {
  const key = `${x},${y}`;
  if (!_coordCache.has(key)) {
    _coordCache.set(key, [x, y] as Coord); // Cast to the branded type
    [minX, minY] = [Math.min(x, minX), Math.min(y, minY)];
    [maxX, maxY] = [Math.max(x, maxX), Math.max(y, maxY)];
  }
  return _coordCache.get(key)!;
};

export const getBounds = () => ({ minX, maxX, minY, maxY });

export const render = (renderFunc: (coord: Coord) => string) => {
  const res = range(minY, maxY + 1)
    .map((y) =>
      range(minX, maxX + 1)
        .map((x) => renderFunc(xy(x, y)))
        .join("")
    )
    .join("\n");
  console.log(res);
};

export const renderFromMap = <T>(
  map: Map<Coord, T>,
  renderFunc: (tile: T | undefined, coord: Coord) => string
) => render((coord) => renderFunc(map.get(coord), coord));

export const offsets: Record<Dir, RawCoord> = {
  u: [0, -1],
  r: [1, 0],
  d: [0, 1],
  l: [-1, 0],
};

export const move = ([x, y]: RawCoord, dir: Dir): Coord => {
  const [dx, dy] = offsets[dir];
  return xy(x + dx, y + dy);
};

export const neighbors = ([x, y]: RawCoord) => {
  return Object.values(offsets).map(([dx, dy]) => xy(x + dx, y + dy));
};

export const mapFromInput = <Tile>(
  input: string,
  toTile: (char: string, coord: Coord) => Tile
): Map<Coord, Tile> => {
  const map = new Map<Coord, Tile>();

  input.split("\n").forEach((line, y) =>
    line.split("").forEach((char, x) => {
      const coord = xy(x, y);
      map.set(coord, toTile(char, coord));
    })
  );

  return map;
};
