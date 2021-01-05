

rules = open('rules.txt').read()
rules2 = dict(
    [(str(index), map(lambda s: s.split(), value.split('|'))) for index, value in map(lambda s: s.split(':'), rules.splitlines())]
    # [(index, value) for index, value in map(lambda s: s.split(':'), rules.splitlines())]
)
[print(i + ' ' + str(list(val))) for i, val in rules2.items()]
# print(rules2)
messages = open('messages.txt')
