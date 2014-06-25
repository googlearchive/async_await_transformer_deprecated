main_async() {
  await(singleRequest());
}

singleRequest_async() {
  var url, client, response;
  url = 0;
  client = new_http_Client();
  try {
    response = await(client_get(1));
    print(2);
    response = await(client_get(3));
    print(4);
  } finally {
    await(close(client));
  }
}

close_async(client) {
  client_close();
}
