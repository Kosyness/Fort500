var server, contents

contents = read_file("tests/project.html")

server = server_create("0.0.0.0:8080")
client = server_accept(server)

write client

server_send(client, contents)