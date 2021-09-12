var my_custom_map, output, inner_map

inner_map = map()

map_set(inner_map, "world", 123456789)

my_custom_map = map()

map_set(my_custom_map, "hello", inner_map)

output = map_get(my_custom_map, "hello")

write output