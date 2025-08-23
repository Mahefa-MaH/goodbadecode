// Good Code: Using Blocks for Asynchronous Operations

#import <Foundation/Foundation.h>

@interface MyObject : NSObject
- (void)performLongRunningTaskWithCompletion:(void (^)(BOOL success))completion;
@end

@implementation MyObject
- (void)performLongRunningTaskWithCompletion:(void (^)(BOOL success))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate a long-running task
        sleep(2); 
        BOOL success = arc4random_uniform(2) == 0; // Simulate success/failure
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(success);
        });
    });
}
@end


int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyObject *obj = [[MyObject alloc] init];
        [obj performLongRunningTaskWithCompletion:^(BOOL success) {
            if (success) {
                NSLog(@"Task completed successfully!");
            } else {
                NSLog(@"Task failed!");
            }
        }];
        [[NSRunLoop currentRunLoop] run]; // Keep the app running to see the result.

    }
    return 0;
}


// Bad Code: Improper Error Handling and Threading

#import <Foundation/Foundation.h>

@interface MyObject : NSObject
- (void)performTask;
@end

@implementation MyObject
- (void)performTask {
    //No error handling
    //Potential race conditions if multiple threads access shared resources.
    NSFileHandle *file = [NSFileHandle fileHandleForReadingAtPath:@"/path/to/file"];
    NSData *data = [file readDataOfLength:1024];
    //Missing error checks
    NSString *str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    NSLog(@"%@", str);
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyObject *obj = [[MyObject alloc] init];
        [obj performTask];
    }
    return 0;
}
