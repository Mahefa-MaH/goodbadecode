// Good Groovy Code: Using builder pattern for fluent API and leveraging closures for concise code.

class PersonBuilder {
    String name
    int age
    String city

    PersonBuilder name(String name) { this.name = name; return this }
    PersonBuilder age(int age) { this.age = age; return this }
    PersonBuilder city(String city) { this.city = city; return this }
    Person build() { new Person(name, age, city) }
}

class Person {
    String name
    int age
    String city

    Person(String name, int age, String city) {
        this.name = name; this.age = age; this.city = city
    }

    String toString() { "Person(name=$name, age=$age, city=$city)" }
}


def person = new PersonBuilder().name('John Doe').age(30).city('New York').build()
println person


// Bad Groovy Code:  Poorly structured, inconsistent style, and lacks error handling.

def badPerson(name,age,city) {
    if(name) {
       def p = [:]
       p.name = name
       if(age) p.age = age
       if(city) p.city = city
       return p
    }
    return null

}

def badPersonInstance = badPerson("Jane Doe", 25, "London")
println badPersonInstance

def badPersonInstance2 = badPerson(null,25,"London")
println badPersonInstance2

