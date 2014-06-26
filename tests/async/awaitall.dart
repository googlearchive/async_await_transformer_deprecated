main_async() {

  var tasks = [
      constant_async(1), 
      constant_async(2), 
      constant_async(3)
  ];

  await(awaitall_async(tasks));

}

constant_async(x){ 
 return x; 
}

awaitall_async(tasks) {
   for(var task in tasks) { 
      await(task); 
   }
}
