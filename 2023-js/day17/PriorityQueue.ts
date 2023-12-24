export class PriorityQueue<T> {
  private data: Array<[number, T]> = [];

  constructor(initValues?: Array<[number, T]>) {
    initValues?.forEach(([priority, val]) => this.insert(priority, val));
  }

  private swap(i1: number, i2: number) {
    const tmp = this.data[i1];
    this.data[i1] = this.data[i2];
    this.data[i2] = tmp;
  }

  insert(priority: number, val: T) {
    const newEntry: [number, T] = [priority, val];
    this.data.push(newEntry);
    this.swim(this.data.length - 1);
  }

  private swim(index: number) {
    const parentI = Math.floor(index / 2);
    if (this.data[parentI][0] > this.data[index][0]) {
      this.swap(index, parentI);
      this.swim(parentI);
    }
  }

  pop(): T | undefined {
    const head = this.data[0];
    // fix the heap
    const tail = this.data.pop();
    if (!tail || tail === head) return head?.[1];

    this.data[0] = tail;
    this.sink(0);

    return head[1];
  }

  private sink(i: number) {
    const childIdx = 2 * i + 1;

    const [[val], child1, child2] = [
      this.data[i],
      this.data[childIdx],
      this.data[childIdx + 1],
    ];
    if (child1 === undefined) return;
    if (val < child1[0] && val < (child2?.[0] ?? Infinity)) return;
    if (child1[0] < (child2?.[0] ?? Infinity)) {
      this.swap(i, childIdx);
      this.sink(childIdx);
    } else if (child2 !== undefined) {
      this.swap(i, childIdx + 1);
      this.sink(childIdx + 1);
    }
  }
}
