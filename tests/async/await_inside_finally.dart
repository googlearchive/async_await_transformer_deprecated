import 'dart:async';
import 'package:http/http.dart' as http;

main() async {
   await singleRequest();
}

singleRequest() async {
   var url = 'http://www.google.com/';
   var client = new http.Client();
   try {
     var response = await client.get('${url}/search');
     print("Response body ${response.statusCode}");
     response = await client.get('${url}/doodles');
     print("Response body ${response.statusCode}");
   } finally {
     await close(client);
   }
}

close(client) async {
  client.close();
}
