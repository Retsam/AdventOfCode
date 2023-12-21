export const all = ["u", "d", "l", "r"] as const;
export type Dir = (typeof all)[number];

export const turnLeft: Record<Dir, Dir> = { l: "d", d: "r", r: "u", u: "l" };
export const turnRight: Record<Dir, Dir> = { l: "u", d: "l", r: "d", u: "r" };
export const reverse: Record<Dir, Dir> = { u: "d", d: "u", l: "r", r: "l" };
