main() async {

  var tasks = [
      constant_async(1), 
      constant_async(2), 
      constant_async(3)
  ];

  await(awaitall_async(tasks));

}

constant (x) async {
 return x; 
}

awaitall (tasks) async {
   for(var task in tasks) { 
      await(task); 
   }
}
