declare class Tagged<Tag extends string> {
  private __tag__: Tag;
}
export type Branded<T, Tag extends string> = T & Tagged<Tag>;

export const range = (start: number, end: number) =>
  Array.from({ length: end - start }, (_, i) => i + start);
