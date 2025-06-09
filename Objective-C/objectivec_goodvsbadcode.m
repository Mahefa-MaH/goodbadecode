// Good Code: Using blocks for asynchronous operations and error handling

@interface MyObject : NSObject

- (void)performOperationWithCompletion:(void (^)(NSData *data, NSError *error))completion;

@end

@implementation MyObject

- (void)performOperationWithCompletion:(void (^)(NSData *data, NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate network operation
        NSData *data = [@"This is some data" dataUsingEncoding:NSUTF8StringEncoding];
        NSError *error = nil;

        //Simulate error condition.
        if (arc4random_uniform(2) == 0) {
            error = [NSError errorWithDomain:@"MyDomain" code:1001 userInfo:@{NSLocalizedDescriptionKey: @"Simulated Error"}];
            data = nil;
        }

        dispatch_async(dispatch_get_main_queue(), ^{
            completion(data, error);
        });
    });
}

@end


// Bad Code: Ignoring error handling and using outdated techniques

@interface MyObjectBad : NSObject
- (NSData*)performOperation;
@end

@implementation MyObjectBad
- (NSData*)performOperation {
    //Simulate network operation.  Error handling completely absent.
    return [@"This is some data" dataUsingEncoding:NSUTF8StringEncoding];
}
@end


// Example usage of Good Code
MyObject *obj = [[MyObject alloc] init];
[obj performOperationWithCompletion:^(NSData *data, NSError *error) {
    if (error) {
        NSLog(@"Error: %@", error);
    } else {
        NSLog(@"Data: %@", [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]);
    }
}];


// Example usage of Bad Code (Illustrative, avoid in production)
MyObjectBad *objBad = [[MyObjectBad alloc] init];
NSData *dataBad = [objBad performOperation];
NSString *strBad = [[NSString alloc] initWithData:dataBad encoding:NSUTF8StringEncoding];
NSLog(@"Data (Bad): %@", strBad);

