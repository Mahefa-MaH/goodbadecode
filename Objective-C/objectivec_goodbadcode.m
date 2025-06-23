// Good Code: Using blocks for asynchronous operations and error handling

#import <Foundation/Foundation.h>

@interface MyObject : NSObject

- (void)performLongRunningTaskWithCompletion:(void (^)(NSData *data, NSError *error))completion;

@end

@implementation MyObject

- (void)performLongRunningTaskWithCompletion:(void (^)(NSData *data, NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate a long-running task
        sleep(2); 
        NSData *data = [@"Success!" dataUsingEncoding:NSUTF8StringEncoding];
        NSError *error = nil;

        //Simulate error condition
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

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyObject *obj = [[MyObject alloc] init];
        [obj performLongRunningTaskWithCompletion:^(NSData *data, NSError *error) {
            if (error) {
                NSLog(@"Error: %@", error);
            } else {
                NSString *result = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                NSLog(@"Result: %@", result);
            }
        }];
        [[NSRunLoop currentRunLoop] run]; //Keep the program running to see output
    }
    return 0;
}


// Bad Code:  Ignoring error handling and using outdated practices

#import <Foundation/Foundation.h>

@interface MyObjectBad : NSObject
- (void)performTask;
@end

@implementation MyObjectBad
- (void)performTask {
    //No error handling, potential for crashes.
    // No asynchronous handling, blocking the main thread.
    [NSThread sleepForTimeInterval:2.0];
    NSLog(@"Task complete (bad example).");
}
@end


int mainBad(int argc, const char * argv[]) {
    @autoreleasepool {
        MyObjectBad *objBad = [[MyObjectBad alloc] init];
        [objBad performTask];
    }
    return 0;
}
