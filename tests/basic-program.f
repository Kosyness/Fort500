string name
integer age
string file_path
string output

print("What is your name?: ")
name = input()

if ( eq("easter egg", name) ) then 
    println("You found the Easter Egg!")
endif

print("Welcome ", name, ". What is your age?: ")
age = input()

println("So you are ", age, " years old, I see!")

print("Where would you like to store that information?: ")
file_path = input()

output = Concat("Name: ", name, ", Age: ", age)
StringToFile(file_path, output)