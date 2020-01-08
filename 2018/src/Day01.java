import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class Day01 {
    public static void main (String[] args) {
        ArrayList<Integer> ls = readInput();
        assert ls != null;
        System.out.println(a(ls));

        System.out.println(b(ls));
    }

    static int b (ArrayList<Integer> ls) {
        HashSet<Integer> set = new HashSet<Integer>();
        int current = 0;
        while (true) {
            boolean bool = true;
            for (Integer l : ls) {
                current += l;
                bool = set.add(current);
                if (!bool) break;
            }
            if (!bool) break;
        }
        return current;
    }

    static int a (ArrayList<Integer> ls) {
        var reduce = ls.stream().reduce(Integer::sum);
        return reduce.orElse(0);
    }

    static ArrayList<Integer> readInput () {
        try {
            return Files.lines(Paths.get("inputs/input1.txt"))
                    .map(Integer::parseInt)
                    .collect(Collectors.toCollection(ArrayList::new));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
}
