proc goodCode(x: int): int =
  if x > 0:
    return x * 2
  else:
    return 0

proc badCode(x: int): int =
  var result: int
  if x > 0:
    result = x * 2
  else:
    result = 0
  return result
