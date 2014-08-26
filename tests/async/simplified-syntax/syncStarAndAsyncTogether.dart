main_async() {

  await(awaitall_async(generate_tasks_syncStar(1,2,3)));

}

generate_tasks_syncStar(a, b, c) {
   yield(constant_async(a));
   yield(constant_async(b));
   yield(constant_async(c))
}

constant_async(x){ return x; }

awaitall_async(tasks) {
   for(var task in tasks) {
      await(task);
   }
}
