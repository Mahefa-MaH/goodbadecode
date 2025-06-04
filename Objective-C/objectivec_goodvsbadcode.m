// Good Code: Using blocks for asynchronous operations and proper error handling

@interface MyObject : NSObject

- (void)performOperationWithCompletion:(void (^)(NSError *error))completion;

@end

@implementation MyObject

- (void)performOperationWithCompletion:(void (^)(NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate some asynchronous operation that might fail
        BOOL success = arc4random_uniform(2) == 0; 
        NSError *error = nil;
        if (!success) {
            error = [NSError errorWithDomain:@"MyDomain" code:1 userInfo:@{NSLocalizedDescriptionKey: @"Operation failed"}];
        }
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(error);
        });
    });
}

@end


// Bad Code:  Lack of error handling, improper memory management, and blocking the main thread

@interface BadObject : NSObject

- (void)performBadOperation;

@end

@implementation BadObject

- (void)performBadOperation {
    //Simulate a long running operation on the main thread, blocking the UI
    for (int i = 0; i < 1000000000; i++) {
       //do something
    }
    //No error handling
}


@end
