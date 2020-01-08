import java.lang.reflect.Array;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

public class Day02 {
    public static void main(String[] args) {
        var input = readInput();
        AtomicInteger two = new AtomicInteger();
        AtomicInteger three = new AtomicInteger();
        input.forEach(s -> {
            boolean[] a = stringFilter(s);
            if (a[0]) two.getAndIncrement();
            if (a[1]) three.getAndIncrement();
        });
        System.out.println(two.get() * three.get());
        System.out.println(compareStrings(input));
    }

    static ArrayList<String> compareStrings (ArrayList<String> ls) {
        var newLs = new ArrayList<String>();
        ls.forEach(s -> {
            ls.forEach(s1 -> {
//                List<Character> as = Arrays.asList(s.toCharArray());
                ArrayList<Character> ass = new ArrayList<Character>(s);
                var as1 = Arrays.asList(s1.toCharArray());
                var union = new ArrayList<Character>(as);
                if (s.compareTo(s1) == 1) {
                    newLs.add(s + s1);
                }

            });
        });
        return newLs;
    }

    static ArrayList<String> readInput () {
        try {
            return Files.lines(Paths.get("inputs/input2.txt"))
                    .collect(Collectors.toCollection(ArrayList::new));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    static boolean[] stringFilter (String s) {
        HashMap<Integer, Integer> map = new HashMap<>();
        s.chars().forEach(chr -> {
            Integer a = map.get(chr);
            map.replace(chr, (a == null ? 1 : a + 1 ));
            if (a == null) {
                map.put(chr, 1);
            } else {
                map.replace(chr, a + 1);
            }
        });
        boolean two = map.containsValue(2);
        boolean three = map.containsValue(3);
        return new boolean[]{two, three};
    }
}
