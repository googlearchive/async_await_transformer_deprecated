main_async() {

  await(awaitany_async(constant_async(1), constant_async(2)));

}

constant_async(x){ return x; }

awaitany_async(a,b) {
   var completer = new Completer()
   try { 
    a.then((x){ try { completer.complete(x); } catch(e) {} } 
    b.then((x){ try { completer.complete(x); } catch(e) {} }
   } finally { 
      return (await(completer.future()));
   }
}
