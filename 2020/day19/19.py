import re

rules = open('rules.txt').read()
rules2 = dict(
    [(str(index), list(map(lambda s: s.split(), value.split('|')))) for index, value in map(lambda s: s.split(':'), rules.splitlines())]
)

def regexString(identifier):
    listOfLists = list(rules2[identifier])
    returnString = '(' if len(listOfLists) > 1 else ''
    for ls in listOfLists:
        for item in ls:
            if item == '"b"': return 'b'
            elif item == '"a"': return 'a'
            else: returnString += regexString(item)
        returnString += '|' if len(listOfLists) > 1 else ''
    returnString += ')' if len(listOfLists) > 1 else ''
    return returnString


regex = re.compile(regexString('0'))

messages = open('messages.txt').read().splitlines()

count = 0
for message in messages:
    if regex.fullmatch(message):
        count += 1

print('Part 1:')
print(count)

def regexString2(identifier):
    listOfLists = list(rules2[identifier])
    returnString = '(' if len(listOfLists) > 1 else ''
    for index, ls in enumerate(listOfLists):
        for item in ls:
            if item == '"b"': return 'b'
            elif item == '"a"': return 'a'
            elif item == '8': returnString += '(' + regexString2('42') + ')+'
            elif item == '11':
                r42 = regexString2('42')
                r31 = regexString2('31')
                returnString += '('
                for i in range(1, 5):
                    val1 = str(r42 * i)
                    val2 = str(r31 * i)
                    returnString += val1 + val2 + '|'
                returnString = returnString[:-1]
                returnString += ')'
            else: returnString += regexString2(item)
        if index < len(listOfLists) - 1:
            returnString += '|'

    returnString += ')' if len(listOfLists) > 1 else ''
    return returnString

regex2 = re.compile(regexString2('0'))
count = 0
for message in messages:
    if regex2.fullmatch(message):
        count += 1

print('Part 2')
print(count)
