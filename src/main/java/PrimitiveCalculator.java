import java.util.*;
import java.util.function.Function;

public class PrimitiveCalculator {
    abstract static class Operation {
        final Function<Integer, Integer> f;

        Operation(Function<Integer, Integer> f) {
            this.f = f;
        }

        int apply(int x) {
            return f.apply(x);
        }

        abstract Optional<Integer> inverse(int y);
    }

    static final class AddOne extends Operation {
        AddOne() {
            super(x -> x + 1);
        }

        @Override
        Optional<Integer> inverse(int y) {
            if (y > 1) {
                return Optional.of(y - 1);
            } else {
                return Optional.empty();
            }
        }
    }

    static final class MulTwo extends Operation {
        MulTwo() {
            super(x -> x * 2);
        }

        @Override
        Optional<Integer> inverse(int y) {
            if (y > 1 && y % 2 == 0) {
                return Optional.of(y / 2);
            } else {
                return Optional.empty();
            }
        }
    }

    static final class MulThree extends Operation {
        MulThree() {
            super(x -> x * 3);
        }

        @Override
        Optional<Integer> inverse(int y) {
            if (y > 1 && y % 3 == 0) {
                return Optional.of(y / 3);
            } else {
                return Optional.empty();
            }
        }
    }

    static class State {
        private final ArrayList<Integer> ways;
        private final ArrayList<Optional<Integer>> parents;

        State(int n) {
            this.ways = new ArrayList<>(n);
            this.parents = new ArrayList<>(n);

            for (int i = 0; i < n; i++) {
                this.ways.add(0);
                this.parents.add(Optional.empty());
            }
        }

        int ways(int i) {
            return ways.get(i - 1);
        }

        Optional<Integer> parent(int i) {
            return parents.get(i - 1);
        }

        State update(int i, int ways, int parent) {
            this.ways.set(i - 1, ways);
            this.parents.set(i - 1, Optional.of(parent));
            return this;
        }
    }

    static class Solution {
        int number;
        int steps;
        List<Integer> inters;

        public Solution(int number, int steps, List<Integer> inters) {
            this.number = number;
            this.steps = steps;
            this.inters = inters;
        }
    }

    private static List<Integer> reconstruct(int n, State state) {
        Optional<Integer> maybeI = state.parent(n);
        List<Integer> inters = new ArrayList<>();
        inters.add(n);

        while (maybeI.isPresent()) {
            int i = maybeI.get();
            inters.add(i);
            maybeI = state.parent(i);
        }

        Collections.reverse(inters);

        return inters;
    }

    static final class TupleInt {
        final int _1;
        final int _2;

        TupleInt(int _1, int _2) {
            this._1 = _1;
            this._2 = _2;
        }
    }

    private static TupleInt bo3Fixed(Operation o1, Operation o2, Operation o3, State state, int y) {
        Optional<Integer> maybeX1 = o1.inverse(y);
        Optional<Integer> maybeX2 = o2.inverse(y);
        Optional<Integer> maybeX3 = o3.inverse(y);

        int ways1 = maybeX1.map(state::ways).orElse(Integer.MAX_VALUE);
        int ways2 = maybeX2.map(state::ways).orElse(Integer.MAX_VALUE);
        int ways3 = maybeX3.map(state::ways).orElse(Integer.MAX_VALUE);

        if (ways1 <= ways2 && ways1 <= ways3) {
            return new TupleInt(maybeX1.get(), ways1 + 1);
        } else if (ways2 <= ways3) {
            return new TupleInt(maybeX2.get(), ways2 + 1);
        } else {
            return new TupleInt(maybeX3.get(), ways3 + 1);
        }
    }

    static Solution solve(int n) {
        State state = new State(n);
        Operation add1 = new AddOne();
        Operation mul2 = new MulTwo();
        Operation mul3 = new MulThree();

        for (int y = 2; y <= n; y++) {
            TupleInt bo3 = bo3Fixed(mul3, mul2, add1, state, y);
            int x = bo3._1;
            int ways = bo3._2;
            state.update(y, ways, x);
        }

        int ways = state.ways(n);
        List<Integer> inters = reconstruct(n, state);
        return new Solution(n, ways, inters);
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
//        long start = System.currentTimeMillis();
        Solution solution = solve(n);
//        long end = System.currentTimeMillis();
//        long time = end - start;
//        System.out.println("Time: " + time);
        System.out.println(solution.steps);
        for (Integer x : solution.inters) {
            System.out.print(x + " ");
        }
    }
}

