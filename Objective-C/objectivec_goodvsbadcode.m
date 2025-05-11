// Good Code: Using blocks for asynchronous operations and error handling

#import <Foundation/Foundation.h>

@interface MyManager : NSObject

- (void)performLongRunningTaskWithCompletion:(void (^)(NSData *data, NSError *error))completion;

@end

@implementation MyManager

- (void)performLongRunningTaskWithCompletion:(void (^)(NSData *data, NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate a long-running task
        sleep(2); 
        NSData *data = [@"This is some data" dataUsingEncoding:NSUTF8StringEncoding];
        NSError *error = nil;

        if (/* some error condition */) {
            error = [NSError errorWithDomain:@"MyDomain" code:1001 userInfo:@{NSLocalizedDescriptionKey: @"Something went wrong"}];
        }

        dispatch_async(dispatch_get_main_queue(), ^{
            completion(data, error);
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
        [[NSRunLoop currentRunLoop] run]; // Keep the main thread running
    }
    return 0;
}



// Bad Code:  Ignoring error handling and using deprecated methods

#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"/path/to/file.txt"; // Replace with a valid path
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil]; //Ignoring error
        NSLog(@"File content: %@", fileContent);
    }
    return 0;
}
