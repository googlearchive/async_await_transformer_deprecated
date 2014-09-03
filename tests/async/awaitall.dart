main() async {
  var tasks = [
      constant(1),
      constant(2),
      constant(3)
  ];
  await awaitall(tasks);
}

constant(x) async => x;

awaitall(tasks) async {
  for (var task in tasks) {
    await task;
  }
}
