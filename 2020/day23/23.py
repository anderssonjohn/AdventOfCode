class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

    def removeNext(self):
        previousNext = self.next
        self.next = previousNext.next
        return previousNext


class LinkedList:
    def __init__(self, data):
        head, *tail = data
        self.head = Node(head)
        self.last = self.head
        self.index = {head: self.head}

        [self.insert(value) for value in tail]

    def insert(self, data):
        nextNode = Node(data)
        self.last.next = nextNode
        self.last = nextNode
        self.index[data] = nextNode

    def toList(self):
        ls = []
        head = self.head
        while head:
            ls.append(head.data)
            head = head.next

        return ls

    def find(self, value):
        return self.index[value]

    def rotateHead(self):
        prevHead = self.head
        self.head = prevHead.next
        self.last.next = prevHead
        self.last = prevHead
        self.last.next = None

    def insertAfter(self, before, after):
        if before == self.last:
            self.last = after
        previousNext = before.next
        before.next = after
        after.next = previousNext

MAX_VALUE = 1000000 + 1
myInput = [4, 7, 6, 1, 3, 8, 2, 5, 9] + list((range(10, MAX_VALUE)))
testInput = [3, 8, 9, 1, 2, 5, 4, 6, 7]
llist = LinkedList(myInput)
testList = LinkedList(testInput)

ls = llist

def moveCups(ls):
    currentCup = ls.head
    r1, r2, r3 = currentCup.removeNext(), currentCup.removeNext(), currentCup.removeNext()
    destinationValue = findDestinationValue(currentCup, r1, r2, r3)
    destinationCup = ls.find(destinationValue)
    ls.insertAfter(destinationCup, r3)
    ls.insertAfter(destinationCup, r2)
    ls.insertAfter(destinationCup, r1)
    ls.rotateHead()

def findDestinationValue(curr, r1, r2, r3):
    removedValues = [r1.data, r2.data, r3.data]
    newValue = curr.data - 1 if curr.data > 1 else MAX_VALUE - 1
    while newValue in removedValues:
        newValue = (newValue - 1) % MAX_VALUE
        if newValue == 0:
            newValue = MAX_VALUE - 1
    return newValue


for i in range(0, 10000000):
    moveCups(ls)

cup1 = ls.find(1)
val1 = cup1.next.data
val2 = cup1.next.next.data
print(val1 * val2)
