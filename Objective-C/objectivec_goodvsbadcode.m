// Good Code: Using Blocks for Asynchronous Operations

@interface MyViewController : UIViewController

@property (nonatomic, strong) NSURLSessionDataTask *dataTask;

@end

@implementation MyViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    NSURLSession *session = [NSURLSession sharedSession];
    NSURL *url = [NSURL URLWithString:@"https://www.example.com"];
    NSURLRequest *request = [NSURLRequest requestWithURL:url];

    self.dataTask = [session dataTaskWithRequest:request completionHandler:^(NSData * _Nullable data, NSURLResponse * _Nullable response, NSError * _Nullable error) {
        dispatch_async(dispatch_get_main_queue(), ^{
            if (error) {
                NSLog(@"Error: %@", error);
            } else {
                NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                NSLog(@"Response: %@", responseString);
            }
        });
    }];

    [self.dataTask resume];
}

- (void)dealloc {
    [self.dataTask cancel];
}

@end


// Bad Code: Ignoring Error Handling and Using Deprecated Methods

@interface MyViewControllerBad : UIViewController

@end

@implementation MyViewControllerBad

- (void)viewDidLoad {
    [super viewDidLoad];

    NSURL *url = [NSURL URLWithString:@"https://www.example.com"];
    NSURLRequest *request = [NSURLRequest requestWithURL:url];
    NSURLConnection *connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
    [connection start];
}


- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    NSLog(@"Response: %@", responseString); //No error handling
}

//Missing other NSURLConnection delegate methods for complete error handling

@end
