// Good Code: Using Blocks for Asynchronous Operations

#import <Foundation/Foundation.h>

@interface MyManager : NSObject

- (void)performLongRunningTaskWithCompletion:(void (^)(NSData *data, NSError *error))completion;

@end

@implementation MyManager

- (void)performLongRunningTaskWithCompletion:(void (^)(NSData *data, NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate a long-running task
        sleep(3); 
        NSData *data = [@"Long-running task completed" dataUsingEncoding:NSUTF8StringEncoding];
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(data, nil);
        });
    });
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyManager *manager = [[MyManager alloc] init];
        [manager performLongRunningTaskWithCompletion:^(NSData *data, NSError *error) {
            if (error) {
                NSLog(@"Error: %@", error);
            } else {
                NSString *result = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                NSLog(@"Result: %@", result);
            }
        }];
        [[NSRunLoop currentRunLoop] run];
    }
    return 0;
}


// Bad Code: Improper Error Handling and Synchronous Network Calls

#import <Foundation/Foundation.h>

@interface BadManager : NSObject
- (NSData *)performSynchronousNetworkRequest;
@end

@implementation BadManager
- (NSData *)performSynchronousNetworkRequest {
    // Simulate a network request.  This blocks the main thread!
    sleep(3);
    return [@"Result from network request (no error handling)" dataUsingEncoding:NSUTF8StringEncoding];
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        BadManager *badManager = [[BadManager alloc] init];
        NSData *data = [badManager performSynchronousNetworkRequest];
        if(data) {
            NSString *result = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
            NSLog(@"Result: %@", result);
        }
    }
    return 0;
}
