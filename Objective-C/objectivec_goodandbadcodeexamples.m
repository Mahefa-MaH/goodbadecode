// Good Code: Using blocks for asynchronous operations and proper error handling

#import <Foundation/Foundation.h>

@interface MyDataFetcher : NSObject

- (void)fetchDataWithCompletion:(void (^)(NSData *data, NSError *error))completion;

@end

@implementation MyDataFetcher

- (void)fetchDataWithCompletion:(void (^)(NSData *data, NSError *error))completion {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate network request
        sleep(2); 
        NSData *data = [@"Sample Data" dataUsingEncoding:NSUTF8StringEncoding];
        NSError *error = nil;
        
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(data, error);
        });
    });
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MyDataFetcher *fetcher = [[MyDataFetcher alloc] init];
        [fetcher fetchDataWithCompletion:^(NSData *data, NSError *error) {
            if (error) {
                NSLog(@"Error: %@", error);
            } else {
                NSString *string = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                NSLog(@"Data: %@", string);
            }
        }];
        [[NSRunLoop currentRunLoop] run]; // Keep the program running until the completion block executes.
    }
    return 0;
}



// Bad Code:  Ignoring errors, improper memory management, and synchronous network calls (blocking the main thread)

#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        //Simulate blocking network call - BAD
        sleep(2);
        NSLog(@"Data Fetched (synchronously and without error handling)");
    }
    return 0;
}
