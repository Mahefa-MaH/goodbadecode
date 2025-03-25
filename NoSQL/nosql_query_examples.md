// Good: MongoDB aggregation pipeline for finding average order value
db.orders.aggregate([
  { $group: { _id: null, avgOrderValue: { $avg: "$total" } } }
])

// Bad: MongoDB find with map-reduce for average order value (inefficient)
db.orders.find({}, {total:1}).map(function(doc){return doc.total;}).reduce(function(a,b){return a+b;})/db.orders.find().count()


// Good: Cassandra query for finding all users in a specific region
SELECT * FROM users WHERE region = 'North America';

// Bad: Cassandra query with inefficient filtering (no index on 'last_login')
SELECT * FROM users WHERE last_login < dateOf('2023-01-01') ALLOW FILTERING;


// Good: ArangoDB AQL query for finding all users with more than 100 friends.
FOR u IN users
  FILTER COUNT_UNIQUE(u.friends) > 100
  RETURN u

// Bad: ArangoDB AQL query with nested loops (inefficient)
FOR u IN users
  FOR f IN u.friends
    FILTER f.active == true
    RETURN u // Inefficient for large datasets

// Good: Amazon DynamoDB query to get all products of a certain category.
// Assumes a primary key of {category: string, product_id:string}
params = {
  TableName: 'products',
  KeyConditionExpression: 'category = :c',
  ExpressionAttributeValues: {
      ':c': {S: 'Electronics'}
  }
};

// Bad: DynamoDB scan to retrieve all products (inefficient for large tables)
params = {
  TableName: 'products',
};

